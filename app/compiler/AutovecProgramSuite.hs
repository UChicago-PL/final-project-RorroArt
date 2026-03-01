module AutovecProgramSuite
  ( VectorizationGoal(..)
  , AutovecTarget(..)
  , autovecTargets
  , runAutovecSuite
  ) where

import AutoVectorizeFinalIR (AutovecStats(..), autovecBaselineKernel)
import BaselineKernelFinalIR (BaselineDebugKeyRef)
import BaselineKernelFinalLowering (lowerBaselineKernelFinalIR)
import Control.Monad (foldM, forM_)
import Control.Monad.State.Strict (StateT, execStateT, liftIO, modify', runState)
import FinalIRBuilder (BuildM, BuilderState(..), addr0, bin, constId, constStmt, freshId, load, memTy, select, store)
import qualified Data.Map.Strict as M
import qualified FinalIR
import ISA (AluOp(..))
import System.Exit (exitFailure)

data VectorizationGoal
  = GoalReady
  | GoalScalarOnly
  | GoalAspirational
  deriving (Show, Eq)

data AutovecTarget = AutovecTarget
  { atName :: String
  , atGoal :: VectorizationGoal
  , atDescription :: String
  , atKernel :: FinalIR.Kernel BaselineDebugKeyRef
  }

autovecTargets :: [AutovecTarget]
autovecTargets =
  [ AutovecTarget
      { atName = "contiguous-add-16"
      , atGoal = GoalReady
      , atDescription = "Simple contiguous map with exact SIMD multiple"
      , atKernel = buildContiguousAddKernel 16
      }
  , AutovecTarget
      { atName = "contiguous-add-18-tail"
      , atGoal = GoalReady
      , atDescription = "Contiguous map with non-multiple trip count (tail loop expected)"
      , atKernel = buildContiguousAddKernel 18
      }
  , AutovecTarget
      { atName = "gather-load"
      , atGoal = GoalReady
      , atDescription = "Indirect load through index array, contiguous store"
      , atKernel = buildGatherKernel 32
      }
  , AutovecTarget
      { atName = "scatter-store"
      , atGoal = GoalReady
      , atDescription = "Contiguous load with indirect scatter store"
      , atKernel = buildScatterKernel 32
      }
  , AutovecTarget
      { atName = "select-even-lanes"
      , atGoal = GoalReady
      , atDescription = "Vectorizable select/mask style arithmetic in loop body"
      , atKernel = buildSelectKernel 24
      }
  , AutovecTarget
      { atName = "contiguous-add-5"
      , atGoal = GoalScalarOnly
      , atDescription = "Trip count < SIMD width (5 < 8), vectorization skipped"
      , atKernel = buildContiguousAddKernel 5
      }
  , AutovecTarget
      { atName = "contiguous-add-0"
      , atGoal = GoalScalarOnly
      , atDescription = "Degenerate empty loop (trip count 0)"
      , atKernel = buildContiguousAddKernel 0
      }
  , AutovecTarget
      { atName = "stride-two"
      , atGoal = GoalAspirational
      , atDescription = "Affine strided access (2*i), desirable future vectorization target"
      , atKernel = buildStrideTwoKernel 32
      }
  , AutovecTarget
      { atName = "prefix-sum-carry"
      , atGoal = GoalAspirational
      , atDescription = "Loop-carried scalar dependence (prefix-style recurrence)"
      , atKernel = buildReductionKernel 32
      }
  , AutovecTarget
      { atName = "tracewrite-in-loop"
      , atGoal = GoalAspirational
      , atDescription = "Loop containing trace effect alongside load/store"
      , atKernel = buildTraceKernel 16
      }
  ]

runAutovecSuite :: IO ()
runAutovecSuite = do
  putStrLn "Autovec FinalIR target suite"
  putStrLn "=========================="
  failures <- execStateT (mapM_ runTarget autovecTargets) []
  if null failures
    then putStrLn "\nSuite result: PASS"
    else do
      putStrLn "\nSuite result: FAIL"
      putStrLn "Failing GoalReady targets:"
      forM_ failures $ \name ->
        putStrLn ("  - " ++ name)
      exitFailure

runTarget :: AutovecTarget -> StateT [String] IO ()
runTarget target = do
  let kernel = atKernel target
      baselineRes = lowerBaselineKernelFinalIR kernel
  liftIO $ do
    putStrLn ""
    putStrLn ("Target: " ++ atName target)
    putStrLn ("Goal:   " ++ show (atGoal target))
    putStrLn ("Desc:   " ++ atDescription target)

  case baselineRes of
    Left err -> do
      liftIO $ putStrLn ("Baseline lower: FAIL (" ++ err ++ ")")
      whenReadyFail target
    Right baselineProg -> do
      liftIO $ putStrLn ("Baseline lower: PASS (bundles=" ++ show (length baselineProg) ++ ")")
      case autovecBaselineKernel kernel of
        Left err -> do
          liftIO $ putStrLn ("Autovec:        FAIL (" ++ err ++ ")")
          whenReadyFail target
        Right (vecKernel, stats) ->
          case lowerBaselineKernelFinalIR vecKernel of
            Left err -> do
              liftIO $ do
                putStrLn ("Autovec stats:  " ++ show stats)
                putStrLn ("Vec lower:      FAIL (" ++ err ++ ")")
              whenReadyFail target
            Right vecProg -> do
              liftIO $ do
                putStrLn ("Autovec stats:  " ++ show stats)
                putStrLn ("Vec lower:      PASS (bundles=" ++ show (length vecProg) ++ ")")
                putStrLn ("Bundle delta:   " ++ show (length baselineProg - length vecProg))
              case atGoal target of
                GoalReady ->
                  if loopsVectorized stats > 0
                    then liftIO (putStrLn "Goal check:     PASS")
                    else do
                      liftIO (putStrLn "Goal check:     FAIL (expected at least one vectorized loop)")
                      whenReadyFail target
                GoalScalarOnly ->
                  if loopsVectorized stats == 0
                    then liftIO (putStrLn "Goal check:     PASS (correctly stayed scalar)")
                    else do
                      liftIO (putStrLn "Goal check:     FAIL (expected no vectorized loops)")
                      whenReadyFail target
                GoalAspirational ->
                  if loopsVectorized stats > 0
                    then liftIO (putStrLn "Goal check:     NOTE (vectorized)")
                    else liftIO (putStrLn "Goal check:     TODO (not vectorized yet)")

whenReadyFail :: AutovecTarget -> StateT [String] IO ()
whenReadyFail target =
  case atGoal target of
    GoalReady -> modify' (atName target :)
    GoalScalarOnly -> modify' (atName target :)
    GoalAspirational -> pure ()

--------------------------------------------------------------------------------
-- Program builders
--------------------------------------------------------------------------------

buildKernelWithMeta
  :: [FinalIR.Ty]
  -> (FinalIR.Id -> [FinalIR.Id] -> BuildM ([FinalIR.Stmt BaselineDebugKeyRef], FinalIR.Id))
  -> FinalIR.Kernel BaselineDebugKeyRef
buildKernelWithMeta metaTys buildBody =
  let initState = BuilderState 0 M.empty []
      ((memIn, stmts, finalMem), st) = runState build initState
      constStmts = map constStmt (reverse (bsConstDefsRev st))
   in FinalIR.Kernel
        { FinalIR.kernelParams = [(memIn, memTy)]
        , FinalIR.kernelRetTys = [memTy]
        , FinalIR.kernelBody =
            FinalIR.Region
              { FinalIR.regionParams = []
              , FinalIR.regionStmts = constStmts ++ stmts
              , FinalIR.regionYield = [finalMem]
              }
        }
  where
    build = do
      memIn <- freshId
      zero <- constId 0
      (metaStmts, metaVals, memAfterMeta) <-
        foldM (loadMeta zero) ([], [], memIn) (zip [0 ..] metaTys)
      (bodyStmts, finalMem) <- buildBody memAfterMeta metaVals
      pure (memIn, metaStmts ++ bodyStmts, finalMem)

loadMeta
  :: FinalIR.Id
  -> ([FinalIR.Stmt BaselineDebugKeyRef], [FinalIR.Id], FinalIR.Id)
  -> (Int, FinalIR.Ty)
  -> BuildM ([FinalIR.Stmt BaselineDebugKeyRef], [FinalIR.Id], FinalIR.Id)
loadMeta zero (stmts, vals, memIn) (ix, ty) = do
  ixConst <- constId ix
  (v, memOut, s) <- load ty memIn (FinalIR.Addr zero (FinalIR.IndexVal ixConst))
  pure (stmts ++ [s], vals ++ [v], memOut)

buildContiguousAddKernel :: Int -> FinalIR.Kernel BaselineDebugKeyRef
buildContiguousAddKernel tripCount =
  buildKernelWithMeta [FinalIR.Ptr, FinalIR.Ptr] $ \mem0 metaVals -> do
    let (inpPtr, outPtr) = expectMeta2 "buildContiguousAddKernel" metaVals
    one <- constId 1
    ub <- constId tripCount

    iv <- freshId
    memIn <- freshId

    (addrIn, s1) <- bin FinalIR.Ptr Add inpPtr iv
    (x, mem1, s2) <- load FinalIR.I32 memIn (addr0 addrIn)
    (y, s3) <- bin FinalIR.I32 Add x one
    (addrOut, s4) <- bin FinalIR.Ptr Add outPtr iv
    (mem2, s5) <- store mem1 (addr0 addrOut) y

    memOut <- freshId
    let loop =
          FinalIR.For
            { FinalIR.forExec = FinalIR.ExecScalar
            , FinalIR.forLb = 0
            , FinalIR.forUb = ub
            , FinalIR.forStep = 1
            , FinalIR.forInits = [mem0]
            , FinalIR.forBody =
                FinalIR.Region
                  { FinalIR.regionParams =
                      [ (iv, FinalIR.I32)
                      , (memIn, memTy)
                      ]
                  , FinalIR.regionStmts = [s1, s2, s3, s4, s5]
                  , FinalIR.regionYield = [mem2]
                  }
            , FinalIR.forOuts = [(memOut, memTy)]
            }

    pure ([loop], memOut)

buildGatherKernel :: Int -> FinalIR.Kernel BaselineDebugKeyRef
buildGatherKernel tripCount =
  buildKernelWithMeta [FinalIR.Ptr, FinalIR.Ptr, FinalIR.Ptr] $ \mem0 metaVals -> do
    let (dataPtr, idxPtr, outPtr) = expectMeta3 "buildGatherKernel" metaVals
    ub <- constId tripCount
    salt <- constId 0x9E3779B9

    iv <- freshId
    memIn <- freshId

    (addrIdx, s1) <- bin FinalIR.Ptr Add idxPtr iv
    (idx, mem1, s2) <- load FinalIR.I32 memIn (addr0 addrIdx)
    (addrData, s3) <- bin FinalIR.Ptr Add dataPtr idx
    (x, mem2, s4) <- load FinalIR.I32 mem1 (addr0 addrData)
    (y, s5) <- bin FinalIR.I32 Xor x salt
    (addrOut, s6) <- bin FinalIR.Ptr Add outPtr iv
    (mem3, s7) <- store mem2 (addr0 addrOut) y

    memOut <- freshId
    let loop =
          FinalIR.For
            { FinalIR.forExec = FinalIR.ExecScalar
            , FinalIR.forLb = 0
            , FinalIR.forUb = ub
            , FinalIR.forStep = 1
            , FinalIR.forInits = [mem0]
            , FinalIR.forBody =
                FinalIR.Region
                  { FinalIR.regionParams =
                      [ (iv, FinalIR.I32)
                      , (memIn, memTy)
                      ]
                  , FinalIR.regionStmts = [s1, s2, s3, s4, s5, s6, s7]
                  , FinalIR.regionYield = [mem3]
                  }
            , FinalIR.forOuts = [(memOut, memTy)]
            }

    pure ([loop], memOut)

buildScatterKernel :: Int -> FinalIR.Kernel BaselineDebugKeyRef
buildScatterKernel tripCount =
  buildKernelWithMeta [FinalIR.Ptr, FinalIR.Ptr, FinalIR.Ptr] $ \mem0 metaVals -> do
    let (inpPtr, idxPtr, outPtr) = expectMeta3 "buildScatterKernel" metaVals
    ub <- constId tripCount

    iv <- freshId
    memIn <- freshId

    (addrIdx, s1) <- bin FinalIR.Ptr Add idxPtr iv
    (idx, mem1, s2) <- load FinalIR.I32 memIn (addr0 addrIdx)
    (addrIn, s3) <- bin FinalIR.Ptr Add inpPtr iv
    (x, mem2, s4) <- load FinalIR.I32 mem1 (addr0 addrIn)
    (addrOut, s5) <- bin FinalIR.Ptr Add outPtr idx
    (mem3, s6) <- store mem2 (addr0 addrOut) x

    memOut <- freshId
    let loop =
          FinalIR.For
            { FinalIR.forExec = FinalIR.ExecScalar
            , FinalIR.forLb = 0
            , FinalIR.forUb = ub
            , FinalIR.forStep = 1
            , FinalIR.forInits = [mem0]
            , FinalIR.forBody =
                FinalIR.Region
                  { FinalIR.regionParams =
                      [ (iv, FinalIR.I32)
                      , (memIn, memTy)
                      ]
                  , FinalIR.regionStmts = [s1, s2, s3, s4, s5, s6]
                  , FinalIR.regionYield = [mem3]
                  }
            , FinalIR.forOuts = [(memOut, memTy)]
            }

    pure ([loop], memOut)

buildSelectKernel :: Int -> FinalIR.Kernel BaselineDebugKeyRef
buildSelectKernel tripCount =
  buildKernelWithMeta [FinalIR.Ptr, FinalIR.Ptr] $ \mem0 metaVals -> do
    let (inpPtr, outPtr) = expectMeta2 "buildSelectKernel" metaVals
    ub <- constId tripCount
    zero <- constId 0
    two <- constId 2

    iv <- freshId
    memIn <- freshId

    (addrIn, s1) <- bin FinalIR.Ptr Add inpPtr iv
    (x, mem1, s2) <- load FinalIR.I32 memIn (addr0 addrIn)
    (modV, s3) <- bin FinalIR.I32 Mod iv two
    (isEven, s4) <- bin FinalIR.I32 Eq_ modV zero
    (y, s5) <- select FinalIR.I32 isEven x zero
    (addrOut, s6) <- bin FinalIR.Ptr Add outPtr iv
    (mem2, s7) <- store mem1 (addr0 addrOut) y

    memOut <- freshId
    let loop =
          FinalIR.For
            { FinalIR.forExec = FinalIR.ExecScalar
            , FinalIR.forLb = 0
            , FinalIR.forUb = ub
            , FinalIR.forStep = 1
            , FinalIR.forInits = [mem0]
            , FinalIR.forBody =
                FinalIR.Region
                  { FinalIR.regionParams =
                      [ (iv, FinalIR.I32)
                      , (memIn, memTy)
                      ]
                  , FinalIR.regionStmts = [s1, s2, s3, s4, s5, s6, s7]
                  , FinalIR.regionYield = [mem2]
                  }
            , FinalIR.forOuts = [(memOut, memTy)]
            }

    pure ([loop], memOut)

buildStrideTwoKernel :: Int -> FinalIR.Kernel BaselineDebugKeyRef
buildStrideTwoKernel tripCount =
  buildKernelWithMeta [FinalIR.Ptr, FinalIR.Ptr] $ \mem0 metaVals -> do
    let (inpPtr, outPtr) = expectMeta2 "buildStrideTwoKernel" metaVals
    ub <- constId tripCount
    two <- constId 2

    iv <- freshId
    memIn <- freshId

    (scaled, s1) <- bin FinalIR.I32 Mul iv two
    (addrIn, s2) <- bin FinalIR.Ptr Add inpPtr scaled
    (x, mem1, s3) <- load FinalIR.I32 memIn (addr0 addrIn)
    (addrOut, s4) <- bin FinalIR.Ptr Add outPtr scaled
    (mem2, s5) <- store mem1 (addr0 addrOut) x

    memOut <- freshId
    let loop =
          FinalIR.For
            { FinalIR.forExec = FinalIR.ExecScalar
            , FinalIR.forLb = 0
            , FinalIR.forUb = ub
            , FinalIR.forStep = 1
            , FinalIR.forInits = [mem0]
            , FinalIR.forBody =
                FinalIR.Region
                  { FinalIR.regionParams =
                      [ (iv, FinalIR.I32)
                      , (memIn, memTy)
                      ]
                  , FinalIR.regionStmts = [s1, s2, s3, s4, s5]
                  , FinalIR.regionYield = [mem2]
                  }
            , FinalIR.forOuts = [(memOut, memTy)]
            }

    pure ([loop], memOut)

buildReductionKernel :: Int -> FinalIR.Kernel BaselineDebugKeyRef
buildReductionKernel tripCount =
  buildKernelWithMeta [FinalIR.Ptr, FinalIR.Ptr] $ \mem0 metaVals -> do
    let (inpPtr, outPtr) = expectMeta2 "buildReductionKernel" metaVals
    ub <- constId tripCount
    accInit <- constId 0

    iv <- freshId
    memIn <- freshId
    accIn <- freshId

    (addrIn, s1) <- bin FinalIR.Ptr Add inpPtr iv
    (x, mem1, s2) <- load FinalIR.I32 memIn (addr0 addrIn)
    (accNext, s3) <- bin FinalIR.I32 Add accIn x
    (addrOut, s4) <- bin FinalIR.Ptr Add outPtr iv
    (mem2, s5) <- store mem1 (addr0 addrOut) accNext

    memOut <- freshId
    accOut <- freshId
    let loop =
          FinalIR.For
            { FinalIR.forExec = FinalIR.ExecScalar
            , FinalIR.forLb = 0
            , FinalIR.forUb = ub
            , FinalIR.forStep = 1
            , FinalIR.forInits = [mem0, accInit]
            , FinalIR.forBody =
                FinalIR.Region
                  { FinalIR.regionParams =
                      [ (iv, FinalIR.I32)
                      , (memIn, memTy)
                      , (accIn, FinalIR.I32)
                      ]
                  , FinalIR.regionStmts = [s1, s2, s3, s4, s5]
                  , FinalIR.regionYield = [mem2, accNext]
                  }
            , FinalIR.forOuts =
                [ (memOut, memTy)
                , (accOut, FinalIR.I32)
                ]
            }

    pure ([loop], memOut)

buildTraceKernel :: Int -> FinalIR.Kernel BaselineDebugKeyRef
buildTraceKernel tripCount =
  buildKernelWithMeta [FinalIR.Ptr, FinalIR.Ptr] $ \mem0 metaVals -> do
    let (inpPtr, outPtr) = expectMeta2 "buildTraceKernel" metaVals
    ub <- constId tripCount

    iv <- freshId
    memIn <- freshId

    (addrIn, s1) <- bin FinalIR.Ptr Add inpPtr iv
    (x, mem1, s2) <- load FinalIR.I32 memIn (addr0 addrIn)
    let s3 = FinalIR.Eff (FinalIR.ETraceWrite x)
    (addrOut, s4) <- bin FinalIR.Ptr Add outPtr iv
    (mem2, s5) <- store mem1 (addr0 addrOut) x

    memOut <- freshId
    let loop =
          FinalIR.For
            { FinalIR.forExec = FinalIR.ExecScalar
            , FinalIR.forLb = 0
            , FinalIR.forUb = ub
            , FinalIR.forStep = 1
            , FinalIR.forInits = [mem0]
            , FinalIR.forBody =
                FinalIR.Region
                  { FinalIR.regionParams =
                      [ (iv, FinalIR.I32)
                      , (memIn, memTy)
                      ]
                  , FinalIR.regionStmts = [s1, s2, s3, s4, s5]
                  , FinalIR.regionYield = [mem2]
                  }
            , FinalIR.forOuts = [(memOut, memTy)]
            }

    pure ([loop], memOut)

expectMeta2 :: String -> [FinalIR.Id] -> (FinalIR.Id, FinalIR.Id)
expectMeta2 ctx vals = case vals of
  [a, b] -> (a, b)
  _ ->
    error
      ( ctx
          ++ ": expected 2 metadata values, got "
          ++ show (length vals)
      )

expectMeta3 :: String -> [FinalIR.Id] -> (FinalIR.Id, FinalIR.Id, FinalIR.Id)
expectMeta3 ctx vals = case vals of
  [a, b, c] -> (a, b, c)
  _ ->
    error
      ( ctx
          ++ ": expected 3 metadata values, got "
          ++ show (length vals)
      )
