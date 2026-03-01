module BaselineKernelFinalIR
  ( BaselineDebugRef(..)
  , BaselineDebugKeyRef(..)
  , baselineRounds
  , baselineBatchSize
  , buildBaselineKernelFinalIR
  , baselineKernelFinalIR
  ) where

import Control.Monad (foldM)
import Control.Monad.State.Strict (runState)
import FinalIRBuilder (BuildM, BuilderState(..), addr0, bin, constId, constStmt, freshId, load, memTy, select, store)
import ISA (AluOp(..))
import qualified Data.Map.Strict as M
import qualified FinalIR

data BaselineDebugRef
  = RefConst !Int
  | RefIv !FinalIR.Id
  deriving (Show, Eq, Ord)

data BaselineDebugKeyRef
  = KeyIdx !BaselineDebugRef !BaselineDebugRef
  | KeyVal !BaselineDebugRef !BaselineDebugRef
  | KeyNodeVal !BaselineDebugRef !BaselineDebugRef
  | KeyHashStage !BaselineDebugRef !BaselineDebugRef !Int
  | KeyHashedVal !BaselineDebugRef !BaselineDebugRef
  | KeyNextIdx !BaselineDebugRef !BaselineDebugRef
  | KeyWrappedIdx !BaselineDebugRef !BaselineDebugRef
  deriving (Show, Eq, Ord)

data KernelEnv = KernelEnv
  { envNNodes :: !FinalIR.Id
  , envForestValuesP :: !FinalIR.Id
  , envInpIndicesP :: !FinalIR.Id
  , envInpValuesP :: !FinalIR.Id
  , envZero :: !FinalIR.Id
  , envOne :: !FinalIR.Id
  , envTwo :: !FinalIR.Id
  }

baselineRounds :: Int
baselineRounds = 16

baselineBatchSize :: Int
baselineBatchSize = 256

baselineKernelFinalIR :: FinalIR.Kernel BaselineDebugKeyRef
baselineKernelFinalIR = buildBaselineKernelFinalIR baselineRounds baselineBatchSize

buildBaselineKernelFinalIR :: Int -> Int -> FinalIR.Kernel BaselineDebugKeyRef
buildBaselineKernelFinalIR rounds batchSize =
  let initState = BuilderState 0 M.empty []
      ((kernelParams, bodyStmts, finalMem), st) = runState (buildKernelBody rounds batchSize) initState
      constStmts = map constStmt (reverse (bsConstDefsRev st))
      bodyRegion =
        FinalIR.Region
          { FinalIR.regionParams = []
          , FinalIR.regionStmts = constStmts ++ bodyStmts
          , FinalIR.regionYield = [finalMem]
          }
   in FinalIR.Kernel
        { FinalIR.kernelParams = kernelParams
        , FinalIR.kernelRetTys = [memTy]
        , FinalIR.kernelBody = bodyRegion
        }

buildKernelBody
  :: Int
  -> Int
  -> BuildM ([(FinalIR.Id, FinalIR.Ty)], [FinalIR.Stmt BaselineDebugKeyRef], FinalIR.Id)
buildKernelBody rounds batchSize = do
  memIn <- freshId
  let kernelParams =
        [ (memIn, memTy) ]

  zeroConst <- constId 0

  (_roundsAddr, mem1, initStmt1) <- loadMeta zeroConst memIn 0 FinalIR.I32
  (nNodesAddr, mem2, initStmt2) <- loadMeta zeroConst mem1 1 FinalIR.I32
  (_batchSizeAddr, mem3, initStmt3) <- loadMeta zeroConst mem2 2 FinalIR.I32
  (_forestHeightAddr, mem4, initStmt4) <- loadMeta zeroConst mem3 3 FinalIR.I32
  (forestValuesPAddr, mem5, initStmt5) <- loadMeta zeroConst mem4 4 FinalIR.Ptr
  (inpIndicesPAddr, mem6, initStmt6) <- loadMeta zeroConst mem5 5 FinalIR.Ptr
  (inpValuesPAddr, mem7, initStmt7) <- loadMeta zeroConst mem6 6 FinalIR.Ptr

  oneConst <- constId 1
  twoConst <- constId 2

  roundsUb <- constId rounds
  batchUb <- constId batchSize

  let env =
        KernelEnv
          { envNNodes = nNodesAddr
          , envForestValuesP = forestValuesPAddr
          , envInpIndicesP = inpIndicesPAddr
          , envInpValuesP = inpValuesPAddr
          , envZero = zeroConst
          , envOne = oneConst
          , envTwo = twoConst
          }

  outerIv <- freshId
  outerMemIn <- freshId
  innerIv <- freshId
  innerMemIn <- freshId

  (innerStmts, innerMemOut) <- buildBatchStmts env outerIv innerIv innerMemIn

  innerForOutMem <- freshId
  let innerForStmt =
        FinalIR.For
          { FinalIR.forExec = FinalIR.ExecScalar
          , FinalIR.forLb = 0
          , FinalIR.forUb = batchUb
          , FinalIR.forStep = 1
          , FinalIR.forInits = [outerMemIn]
          , FinalIR.forBody =
              FinalIR.Region
                { FinalIR.regionParams =
                    [ (innerIv, FinalIR.I32)
                    , (innerMemIn, memTy)
                    ]
                , FinalIR.regionStmts = innerStmts
                , FinalIR.regionYield = [innerMemOut]
                }
          , FinalIR.forOuts = [(innerForOutMem, memTy)]
          }

  outerForOutMem <- freshId
  let outerForStmt =
        FinalIR.For
          { FinalIR.forExec = FinalIR.ExecScalar
          , FinalIR.forLb = 0
          , FinalIR.forUb = roundsUb
          , FinalIR.forStep = 1
          , FinalIR.forInits = [mem7]
          , FinalIR.forBody =
              FinalIR.Region
                { FinalIR.regionParams =
                    [ (outerIv, FinalIR.I32)
                    , (outerMemIn, memTy)
                    ]
                , FinalIR.regionStmts = [innerForStmt]
                , FinalIR.regionYield = [innerForOutMem]
                }
          , FinalIR.forOuts = [(outerForOutMem, memTy)]
          }

  let bodyStmts =
        [ initStmt1
        , initStmt2
        , initStmt3
        , initStmt4
        , initStmt5
        , initStmt6
        , initStmt7
        , FinalIR.Eff FinalIR.EPause
        , FinalIR.Eff (FinalIR.EDebugComment "Starting loop")
        , outerForStmt
        , FinalIR.Eff FinalIR.EPause
        ]

  pure (kernelParams, bodyStmts, outerForOutMem)

buildBatchStmts
  :: KernelEnv
  -> FinalIR.Id
  -> FinalIR.Id
  -> FinalIR.Id
  -> BuildM ([FinalIR.Stmt BaselineDebugKeyRef], FinalIR.Id)
buildBatchStmts env roundIv batchIv memIn = do
  let roundRef = RefIv roundIv
      batchRef = RefIv batchIv

  (addrIdx, s1) <- bin FinalIR.Ptr Add (envInpIndicesP env) batchIv
  (tmpIdx, mem1, s2) <- load FinalIR.I32 memIn (addr0 addrIdx)
  let d1 = FinalIR.Eff (FinalIR.EDebugCompare tmpIdx (KeyIdx roundRef batchRef))

  (addrVal, s3) <- bin FinalIR.Ptr Add (envInpValuesP env) batchIv
  (tmpVal0, mem2, s4) <- load FinalIR.I32 mem1 (addr0 addrVal)
  let d2 = FinalIR.Eff (FinalIR.EDebugCompare tmpVal0 (KeyVal roundRef batchRef))

  (addrNode, s5) <- bin FinalIR.Ptr Add (envForestValuesP env) tmpIdx
  (tmpNodeVal, mem3, s6) <- load FinalIR.I32 mem2 (addr0 addrNode)
  let d3 = FinalIR.Eff (FinalIR.EDebugCompare tmpNodeVal (KeyNodeVal roundRef batchRef))

  (tmpVal1, s7) <- bin FinalIR.I32 Xor tmpVal0 tmpNodeVal
  (hashStmts, tmpHashed) <- buildHashStmts roundRef batchRef tmpVal1
  let d4 = FinalIR.Eff (FinalIR.EDebugCompare tmpHashed (KeyHashedVal roundRef batchRef))

  (tmpMod, s8) <- bin FinalIR.I32 Mod tmpHashed (envTwo env)
  (tmpEq, s9) <- bin FinalIR.I32 Eq_ tmpMod (envZero env)
  (tmpSel, s10) <- select FinalIR.I32 tmpEq (envOne env) (envTwo env)
  (tmpMul, s11) <- bin FinalIR.I32 Mul tmpIdx (envTwo env)
  (tmpNextIdx, s12) <- bin FinalIR.I32 Add tmpMul tmpSel
  let d5 = FinalIR.Eff (FinalIR.EDebugCompare tmpNextIdx (KeyNextIdx roundRef batchRef))

  (tmpLt, s13) <- bin FinalIR.I32 Lt tmpNextIdx (envNNodes env)
  (tmpWrappedIdx, s14) <- select FinalIR.I32 tmpLt tmpNextIdx (envZero env)
  let d6 = FinalIR.Eff (FinalIR.EDebugCompare tmpWrappedIdx (KeyWrappedIdx roundRef batchRef))

  (addrStoreIdx, s15) <- bin FinalIR.Ptr Add (envInpIndicesP env) batchIv
  (mem4, s16) <- store mem3 (addr0 addrStoreIdx) tmpWrappedIdx

  (addrStoreVal, s17) <- bin FinalIR.Ptr Add (envInpValuesP env) batchIv
  (mem5, s18) <- store mem4 (addr0 addrStoreVal) tmpHashed

  let stmts =
        [ s1
        , s2
        , d1
        , s3
        , s4
        , d2
        , s5
        , s6
        , d3
        , s7
        ]
          ++ hashStmts
          ++ [ d4
             , s8
             , s9
             , s10
             , s11
             , s12
             , d5
             , s13
             , s14
             , d6
             , s15
             , s16
             , s17
             , s18
             ]

  pure (stmts, mem5)

hashStages :: [(AluOp, Int, AluOp, AluOp, Int)]
hashStages =
  [ (Add, 0x7ED55D16, Add, Shl, 12)
  , (Xor, 0xC761C23C, Xor, Shr, 19)
  , (Add, 0x165667B1, Add, Shl, 5)
  , (Add, 0xD3A2646C, Xor, Shl, 9)
  , (Add, 0xFD7046C5, Add, Shl, 3)
  , (Xor, 0xB55A4F09, Xor, Shr, 16)
  ]

buildHashStmts
  :: BaselineDebugRef
  -> BaselineDebugRef
  -> FinalIR.Id
  -> BuildM ([FinalIR.Stmt BaselineDebugKeyRef], FinalIR.Id)
buildHashStmts roundRef batchRef startVal = do
  foldM step ([], startVal) (zip [0 ..] hashStages)
  where
    step (acc, curVal) (stageI, (op1, val1, op2, op3, val3)) = do
      val1Const <- constId val1
      (tmp1, s1) <- bin FinalIR.I32 op1 curVal val1Const
      val3Const <- constId val3
      (tmp2, s2) <- bin FinalIR.I32 op3 curVal val3Const
      (nextVal, s3) <- bin FinalIR.I32 op2 tmp1 tmp2
      let dbg = FinalIR.Eff (FinalIR.EDebugCompare nextVal (KeyHashStage roundRef batchRef stageI))
      pure (acc ++ [s1, s2, s3, dbg], nextVal)

loadMeta
  :: FinalIR.Id
  -> FinalIR.Id
  -> Int
  -> FinalIR.Ty
  -> BuildM (FinalIR.Id, FinalIR.Id, FinalIR.Stmt k)
loadMeta metaBase memIn ix ty = do
  ixConst <- constId ix
  load ty memIn (FinalIR.Addr metaBase (FinalIR.IndexVal ixConst))

