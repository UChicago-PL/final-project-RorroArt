module FinalLowering
  ( lowerKernel
  , lowerKernelWithDebug
  ) where

import Control.Monad (foldM, forM_, when)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Word (Word32)
import ISA
  ( AluOp(..)
  , AluSlot(..)
  , Bundle(..)
  , DebugSlot(..)
  , FlowSlot(..)
  , LoadSlot(..)
  , Offset(..)
  , ScratchAddr(..)
  , StoreSlot(..)
  , ValuSlot(..)
  )
import FinalIR
  ( Addr(..)
  , Effect(..)
  , Exec(..)
  , Id(..)
  , Index(..)
  , Ix(..)
  , Kernel(..)
  , Region(..)
  , Rhs(..)
  , Stmt(..)
  , Ty(..)
  , Width(..)
  )

data Binding = Binding
  { bTy :: !Ty
  , bScratch :: !ScratchAddr
  , bConst :: !(Maybe Word32)
  }

data BuildState = BuildState
  { bsEnv :: !(Map Id Binding)
  , bsNextTemp :: !Int
  , bsConstTemps :: !(Map Word32 ScratchAddr)
  }

data LowerCtx kIn kOut = LowerCtx
  { lcIdTy :: !(Map Id Ty)
  , lcIdScratch :: !(Map Id ScratchAddr)
  , lcDebugMapFn :: !(Map Id Int -> kIn -> Either String kOut)
  }

lowerKernel :: Kernel k -> Either String [Bundle k]
lowerKernel = lowerKernelWithDebug (\_ k -> Right k)

lowerKernelWithDebug :: (Map Id Int -> kIn -> Either String kOut) -> Kernel kIn -> Either String [Bundle kOut]
lowerKernelWithDebug dbgMapFn kernel = do
  idTy <- collectKernelTypes kernel
  idScratch <- allocateIdScratch idTy
  nextTemp0 <- totalSlots idTy
  when
    (not (null (regionParams (kernelBody kernel))))
    (Left "Kernel body regionParams must be empty; use kernelParams for entry values")
  let ctx = LowerCtx idTy idScratch dbgMapFn
      params = kernelParams kernel
      env0 = M.fromList [ (pid, Binding pTy (requireIdScratch ctx pid) Nothing) | (pid, pTy) <- params ]
      st0 = BuildState env0 nextTemp0 M.empty
  (_st1, bundles, retVals) <- lowerRegion ctx st0 (kernelBody kernel) []
  validateKernelReturns (kernelRetTys kernel) retVals
  Right bundles

validateKernelReturns :: [Ty] -> [Binding] -> Either String ()
validateKernelReturns retTys retVals = do
  when
    (length retTys /= length retVals)
    ( Left $
        "Kernel return arity mismatch: kernelRetTys has "
          ++ show (length retTys)
          ++ ", region yields "
          ++ show (length retVals)
    )
  forM_
    (zip3 [0 :: Int ..] retTys retVals)
    (\(ix, expectedTy, gotB) ->
        when
          (expectedTy /= bTy gotB)
          ( Left $
              "Kernel return type mismatch at index "
                ++ show ix
                ++ ": expected "
                ++ show expectedTy
                ++ ", got "
                ++ show (bTy gotB)
          )
    )

lowerRegion
  :: LowerCtx kIn kOut
  -> BuildState
  -> Region kIn
  -> [Binding]
  -> Either String (BuildState, [Bundle kOut], [Binding])
lowerRegion ctx st0 region args = do
  let params = regionParams region
  when
    (length params /= length args)
    ( Left $
        "Region parameter arity mismatch: expected "
          ++ show (length params)
          ++ ", got "
          ++ show (length args)
    )
  let outerEnv = bsEnv st0
  (st1, paramBundles, ()) <- bindRegionParams ctx st0 params args
  (st2, stmtBundles, ()) <- lowerStmts ctx st1 (regionStmts region)
  yields <- mapM (lookupBinding (bsEnv st2) "region yield") (regionYield region)
  let stOut = st2 {bsEnv = outerEnv}
  Right (stOut, paramBundles ++ stmtBundles, yields)

bindRegionParams
  :: LowerCtx kIn kOut
  -> BuildState
  -> [(Id, Ty)]
  -> [Binding]
  -> Either String (BuildState, [Bundle kOut], ())
bindRegionParams _ st0 params args = go st0 params args
  where
    go st [] [] = Right (st, [], ())
    go st ((pid, pTy) : ps) (arg : as) = do
      when
        (pTy /= bTy arg)
        ( Left $
            "Region parameter type mismatch for "
              ++ show pid
              ++ ": expected "
              ++ show pTy
              ++ ", got "
              ++ show (bTy arg)
        )
      let pBind = Binding pTy (bScratch arg) (bConst arg)
      go (st {bsEnv = M.insert pid pBind (bsEnv st)}) ps as
    go _ _ _ = Left "Internal error: bindRegionParams arity mismatch"

lowerStmts
  :: LowerCtx kIn kOut
  -> BuildState
  -> [Stmt kIn]
  -> Either String (BuildState, [Bundle kOut], ())
lowerStmts ctx st0 stmts = go st0 [] stmts
  where
    go st acc [] = Right (st, reverse acc, ())
    go st acc (stmt : rest) = do
      (st1, stmtBundles, ()) <- lowerStmt ctx st stmt
      go st1 (reverse stmtBundles ++ acc) rest

lowerStmt
  :: LowerCtx kIn kOut
  -> BuildState
  -> Stmt kIn
  -> Either String (BuildState, [Bundle kOut], ())
lowerStmt ctx st stmt = case stmt of
  Let outs rhs -> lowerLet ctx st outs rhs
  Eff eff -> lowerEffect ctx st eff
  If cond thenRegion elseRegion outs -> lowerIf ctx st cond thenRegion elseRegion outs
  For exec lb ub step inits body outs -> lowerFor ctx st exec lb ub step inits body outs

lowerLet
  :: LowerCtx kIn kOut
  -> BuildState
  -> [(Id, Ty)]
  -> Rhs
  -> Either String (BuildState, [Bundle kOut], ())
lowerLet ctx st outs rhs = case rhs of
  RLoad memId addr -> lowerLoadLet ctx st outs memId addr
  RStore memId addr valId -> lowerStoreLet ctx st outs memId addr valId
  _ -> do
    when
      (length outs /= 1)
      ( Left $
          "Let expects exactly one output for "
            ++ show rhs
            ++ "; got "
            ++ show (length outs)
      )
    let (outId, outTy) = head outs
    outBind <- bindingForId ctx outId outTy Nothing
    (st1, rhsBundles, rhsVal) <- lowerRhs ctx st rhs outBind
    let st2 = st1 {bsEnv = M.insert outId rhsVal (bsEnv st1)}
    Right (st2, rhsBundles, ())

lowerLoadLet
  :: LowerCtx kIn kOut
  -> BuildState
  -> [(Id, Ty)]
  -> Id
  -> Addr
  -> Either String (BuildState, [Bundle kOut], ())
lowerLoadLet ctx st outs memId addr = do
  when
    (length outs < 1 || length outs > 2)
    ( Left $
        "RLoad expects 1 or 2 outputs (value[, memOut]); got "
          ++ show (length outs)
    )
  memTok <- lookupBinding (bsEnv st) "RLoad mem token" memId
  ensureMemTy "RLoad mem token" (bTy memTok)
  let (valOutId, valOutTy) = head outs
  when
    (isMemTy valOutTy)
    (Left "RLoad value output cannot have Mem type")
  valOut <- bindingForId ctx valOutId valOutTy Nothing
  memOutMay <-
    case tail outs of
      [] -> Right Nothing
      [(memOutId, memOutTy)] -> do
        ensureMemTy "RLoad memOut" memOutTy
        memOut <- bindingForId ctx memOutId memOutTy Nothing
        Right (Just (memOutId, memOut))
      _ ->
        Left "Internal error: validated RLoad outs but found unexpected shape"
  (st1, addrBundles, addrScratch, _) <- lowerAddr ctx st addr
  let slot =
        if isVectorLikeTy (bTy valOut)
          then VLoad (bScratch valOut) addrScratch
          else Load (bScratch valOut) addrScratch
      env1 = M.insert valOutId (valOut {bConst = Nothing}) (bsEnv st1)
      env2 = case memOutMay of
        Nothing -> env1
        Just (memOutId, memOut) -> M.insert memOutId (memOut {bConst = Nothing}) env1
  Right (st1 {bsEnv = env2}, addrBundles ++ [bundleLoad slot], ())

lowerStoreLet
  :: LowerCtx kIn kOut
  -> BuildState
  -> [(Id, Ty)]
  -> Id
  -> Addr
  -> Id
  -> Either String (BuildState, [Bundle kOut], ())
lowerStoreLet ctx st outs memId addr valId = do
  when
    (length outs > 1)
    ( Left $
        "RStore expects at most 1 output (memOut); got "
          ++ show (length outs)
    )
  memTok <- lookupBinding (bsEnv st) "RStore mem token" memId
  ensureMemTy "RStore mem token" (bTy memTok)
  val <- lookupBinding (bsEnv st) "RStore value" valId
  (st1, addrBundles, addrScratch, _) <- lowerAddr ctx st addr
  let slot =
        if isVectorLikeTy (bTy val)
          then VStore addrScratch (bScratch val)
          else Store addrScratch (bScratch val)
  st2 <-
    case outs of
      [] -> Right st1
      [(memOutId, memOutTy)] -> do
        ensureMemTy "RStore memOut" memOutTy
        memOut <- bindingForId ctx memOutId memOutTy Nothing
        let env1 = M.insert memOutId (memOut {bConst = Nothing}) (bsEnv st1)
        Right (st1 {bsEnv = env1})
      _ ->
        Left "Internal error: validated RStore outs but found unexpected shape"
  Right (st2, addrBundles ++ [bundleStore slot], ())

lowerRhs
  :: LowerCtx kIn kOut
  -> BuildState
  -> Rhs
  -> Binding
  -> Either String (BuildState, [Bundle kOut], Binding)
lowerRhs _ctx st rhs outBind = case rhs of
  RConst w -> do
    ensureScalarValueTy "RConst" (bTy outBind)
    let bOut = outBind {bConst = Just w}
    Right (st, [bundleLoad (Const (bScratch outBind) w)], bOut)

  RBin op aId bId -> do
    a <- lookupBinding (bsEnv st) "RBin lhs" aId
    b <- lookupBinding (bsEnv st) "RBin rhs" bId
    validateBinTypes op (bTy outBind) (bTy a) (bTy b)
    let slotBundle =
          if isVectorLikeTy (bTy outBind)
            then bundleValu (VAlu op (bScratch outBind) (bScratch a) (bScratch b))
            else bundleAlu (Alu op (bScratch outBind) (bScratch a) (bScratch b))
        bOut = outBind {bConst = evalBinConst op (bConst a) (bConst b)}
    Right (st, [slotBundle], bOut)

  RSelect cId aId bId -> do
    c <- lookupBinding (bsEnv st) "RSelect cond" cId
    a <- lookupBinding (bsEnv st) "RSelect then" aId
    b <- lookupBinding (bsEnv st) "RSelect else" bId
    when
      (bTy a /= bTy outBind || bTy b /= bTy outBind)
      ( Left $
          "RSelect type mismatch: output is "
            ++ show (bTy outBind)
            ++ ", then is "
            ++ show (bTy a)
            ++ ", else is "
            ++ show (bTy b)
      )
    if isVectorLikeTy (bTy outBind)
      then validateVectorCondTy "RSelect" (bTy c) (bTy outBind)
      else ensureScalarCondTy "RSelect" (bTy c)
    let flow =
          if isVectorLikeTy (bTy outBind)
            then VSelect (bScratch outBind) (bScratch c) (bScratch a) (bScratch b)
            else Select (bScratch outBind) (bScratch c) (bScratch a) (bScratch b)
        cOut = case bConst c of
          Just 0 -> bConst b
          Just _ -> bConst a
          Nothing -> Nothing
        bOut = outBind {bConst = cOut}
    Right (st, [bundleFlow flow], bOut)

  RCoreId -> do
    ensureScalarValueTy "RCoreId" (bTy outBind)
    Right (st, [bundleFlow (CoreId (bScratch outBind))], outBind {bConst = Nothing})

  RBroadcast rhsW srcId -> do
    src <- lookupBinding (bsEnv st) "RBroadcast source" srcId
    (outW, elemTy) <- expectVecTy "RBroadcast output" (bTy outBind)
    ensureSupportedWidth "RBroadcast" outW
    when
      (rhsW /= outW)
      ( Left $
          "RBroadcast width mismatch: rhs width "
            ++ show rhsW
            ++ ", output width "
            ++ show outW
      )
    when
      (bTy src /= elemTy)
      ( Left $
          "RBroadcast source type mismatch: expected "
            ++ show elemTy
            ++ ", got "
            ++ show (bTy src)
      )
    Right
      ( st
      , [bundleValu (VBroadcast (bScratch outBind) (bScratch src))]
      , outBind {bConst = Nothing}
      )

  RMultiplyAdd aId bId cId -> do
    a <- lookupBinding (bsEnv st) "RMultiplyAdd a" aId
    b <- lookupBinding (bsEnv st) "RMultiplyAdd b" bId
    c <- lookupBinding (bsEnv st) "RMultiplyAdd c" cId
    case bTy outBind of
      Vec w _ -> do
        ensureSupportedWidth "RMultiplyAdd" w
        when
          (bTy a /= bTy outBind || bTy b /= bTy outBind || bTy c /= bTy outBind)
          ( Left $
              "RMultiplyAdd type mismatch: output is "
                ++ show (bTy outBind)
                ++ ", inputs are "
                ++ show (bTy a)
                ++ ", "
                ++ show (bTy b)
                ++ ", "
                ++ show (bTy c)
          )
        Right
          ( st
          , [bundleValu (MultiplyAdd (bScratch outBind) (bScratch a) (bScratch b) (bScratch c))]
          , outBind {bConst = Nothing}
          )
      _ ->
        Left $
          "RMultiplyAdd expects vector output, got "
            ++ show (bTy outBind)

  RLoad {} ->
    Left "Internal error: RLoad should be handled by lowerLoadLet"

  RStore {} ->
    Left "Internal error: RStore should be handled by lowerStoreLet"

lowerEffect
  :: LowerCtx kIn kOut
  -> BuildState
  -> Effect kIn
  -> Either String (BuildState, [Bundle kOut], ())
lowerEffect ctx st eff = case eff of
  EPause ->
    Right (st, [bundleFlow Pause], ())

  ETraceWrite valId -> do
    val <- lookupBinding (bsEnv st) "ETraceWrite" valId
    ensureScalarValueTy "ETraceWrite" (bTy val)
    Right (st, [bundleFlow (TraceWrite (bScratch val))], ())

  EDebugCompare valId kIn -> do
    val <- lookupBinding (bsEnv st) "EDebugCompare" valId
    ensureScalarValueTy "EDebugCompare" (bTy val)
    kOut <- lcDebugMapFn ctx (debugMapFromEnv (bsEnv st)) kIn
    Right (st, [bundleDebug (Compare (bScratch val) kOut)], ())

  EDebugCompareV valId ksIn -> do
    val <- lookupBinding (bsEnv st) "EDebugCompareV" valId
    (w, _) <- vectorLikeWidth "EDebugCompareV value" (bTy val)
    ensureSupportedWidth "EDebugCompareV" w
    when
      (widthToInt w /= length ksIn)
      ( Left $
          "EDebugCompareV key count mismatch: value width "
            ++ show (widthToInt w)
            ++ ", keys length "
            ++ show (length ksIn)
      )
    ksOut <- mapM (lcDebugMapFn ctx (debugMapFromEnv (bsEnv st))) ksIn
    Right (st, [bundleDebug (VCompare (bScratch val) ksOut)], ())

  EDebugComment msg ->
    Right (st, [bundleDebug (DebugIgnored msg)], ())

  EHalt ->
    Right (st, [bundleFlow Halt], ())

lowerIf
  :: LowerCtx kIn kOut
  -> BuildState
  -> Id
  -> Region kIn
  -> Region kIn
  -> [(Id, Ty)]
  -> Either String (BuildState, [Bundle kOut], ())
lowerIf ctx st condId thenRegion elseRegion outs = do
  cond <- lookupBinding (bsEnv st) "If condition" condId
  ensureScalarCondTy "If condition" (bTy cond)
  outBinds <- mapM (\(oid, ty) -> bindingForId ctx oid ty Nothing) outs
  case bConst cond of
    Just 0 -> do
      (stElse, elseBundles, elseYields) <- lowerRegion ctx st elseRegion []
      (stOuts, copyBundles, outVals) <- copyOutputs ctx stElse outBinds elseYields
      let stFinal = insertOutputs (map fst outs) outVals stOuts
      Right (stFinal, elseBundles ++ copyBundles, ())
    Just _ -> do
      (stThen, thenBundles, thenYields) <- lowerRegion ctx st thenRegion []
      (stOuts, copyBundles, outVals) <- copyOutputs ctx stThen outBinds thenYields
      let stFinal = insertOutputs (map fst outs) outVals stOuts
      Right (stFinal, thenBundles ++ copyBundles, ())
    Nothing -> do
      (stThen, thenBundles, thenYields) <- lowerRegion ctx st thenRegion []
      (stElse, elseBundles, elseYields) <- lowerRegion ctx stThen elseRegion []
      validateIfBranch "then" outBinds thenYields
      validateIfBranch "else" outBinds elseYields
      (stAfterElseCopies, elseCopyBundles, elseOuts) <- copyOutputs ctx stElse outBinds elseYields
      (stAfterThenCopies, thenCopyBundles, thenOuts) <- copyOutputs ctx stAfterElseCopies outBinds thenYields
      (stWithTrue, trueConstBundles, trueScratch) <- ensureConstScratch stAfterThenCopies 1
      let elseCode = elseBundles ++ elseCopyBundles
          thenCode = thenBundles ++ thenCopyBundles
          jumpToThen = bundleFlow (CondJumpRel (bScratch cond) (Offset (length elseCode + 1)))
          jumpPastThen = bundleFlow (CondJumpRel trueScratch (Offset (length thenCode)))
          mergedOuts = mergeIfConstants elseOuts thenOuts
          stFinal = insertOutputs (map fst outs) mergedOuts stWithTrue
          code = trueConstBundles ++ [jumpToThen] ++ elseCode ++ [jumpPastThen] ++ thenCode
      Right (stFinal, code, ())

validateIfBranch :: String -> [Binding] -> [Binding] -> Either String ()
validateIfBranch branchName outs ys = do
  when
    (length outs /= length ys)
    ( Left $
        "If "
          ++ branchName
          ++ " yield arity mismatch: expected "
          ++ show (length outs)
          ++ ", got "
          ++ show (length ys)
    )
  forM_ (zip3 [0 :: Int ..] outs ys) (\(ix, outB, y) ->
    when
      (bTy outB /= bTy y)
      ( Left $
          "If "
            ++ branchName
            ++ " type mismatch at index "
            ++ show ix
            ++ ": output is "
            ++ show (bTy outB)
            ++ ", yielded value is "
            ++ show (bTy y)
      )
    )

copyOutputs
  :: LowerCtx kIn kOut
  -> BuildState
  -> [Binding]
  -> [Binding]
  -> Either String (BuildState, [Bundle kOut], [Binding])
copyOutputs ctx st0 outs ys = go st0 [] [] outs ys
  where
    go st bundles acc [] [] = Right (st, reverse bundles, reverse acc)
    go st bundles acc (outB : outRest) (y : yRest) = do
      when
        (bTy outB /= bTy y)
        ( Left $
            "Output copy type mismatch: output is "
              ++ show (bTy outB)
              ++ ", yielded value is "
              ++ show (bTy y)
        )
      let outV = outB {bConst = bConst y}
      (st1, copyBundles, ()) <- emitCopyValue ctx st y outV
      go st1 (reverse copyBundles ++ bundles) (outV : acc) outRest yRest
    go _ _ _ _ _ =
      Left "Output copy arity mismatch"

insertOutputs :: [Id] -> [Binding] -> BuildState -> BuildState
insertOutputs outIds outVals st =
  st
    { bsEnv =
        foldl
          (\env (oid, val) -> M.insert oid val env)
          (bsEnv st)
          (zip outIds outVals)
    }

mergeIfConstants :: [Binding] -> [Binding] -> [Binding]
mergeIfConstants elseVals thenVals =
  zipWith
    (\e t ->
        if bConst e == bConst t
          then e {bConst = bConst e}
          else e {bConst = Nothing}
    )
    elseVals
    thenVals

lowerFor
  :: LowerCtx kIn kOut
  -> BuildState
  -> Exec
  -> Int
  -> Id
  -> Int
  -> [Id]
  -> Region kIn
  -> [(Id, Ty)]
  -> Either String (BuildState, [Bundle kOut], ())
lowerFor ctx st exec lb ubId step inits body outs = do
  validateExec exec
  when (step == 0) (Left "For loop step cannot be 0")
  ub <- lookupBinding (bsEnv st) "For upper bound" ubId
  ensureScalarValueTy "For upper bound" (bTy ub)
  ubInt <- case bConst ub of
    Nothing ->
      Left $
        "For upper bound id "
          ++ show ubId
          ++ " is not compile-time constant; cannot unroll"
    Just w -> Right (fromIntegral w :: Int)

  when
    (length outs /= length inits)
    ( Left $
        "For arity mismatch: forOuts has "
          ++ show (length outs)
          ++ ", forInits has "
          ++ show (length inits)
    )

  when
    (length (regionParams body) /= length inits + 1)
    ( Left $
        "For body parameter mismatch: expected "
          ++ show (length inits + 1)
          ++ " params (induction + carries), got "
          ++ show (length (regionParams body))
    )

  initBinds <- mapM (lookupBinding (bsEnv st) "For init") inits
  outBinds <- mapM (\(oid, ty) -> bindingForId ctx oid ty Nothing) outs

  let (ivId, ivTy) = head (regionParams body)
      carryParams = tail (regionParams body)
      tripVals = forTripValues lb ubInt step

  ensureScalarValueTy "For induction variable" ivTy

  forM_ (zip3 [0 :: Int ..] carryParams initBinds) (\(ix, (_, pTy), initB) ->
    when
      (pTy /= bTy initB)
      ( Left $
          "For carry type mismatch at index "
            ++ show ix
            ++ ": body param is "
            ++ show pTy
            ++ ", init is "
            ++ show (bTy initB)
      )
    )

  (stAfterLoop, loopBundles, finalCarries) <- lowerForIters ctx st (ivId, ivTy) body tripVals initBinds

  when
    (length finalCarries /= length outBinds)
    ( Left $
        "For body yield arity mismatch: expected "
          ++ show (length outBinds)
          ++ ", got "
          ++ show (length finalCarries)
    )

  forM_ (zip3 [0 :: Int ..] outBinds finalCarries) (\(ix, outB, cB) ->
    when
      (bTy outB /= bTy cB)
      ( Left $
          "For output type mismatch at index "
            ++ show ix
            ++ ": forOut is "
            ++ show (bTy outB)
            ++ ", carried value is "
            ++ show (bTy cB)
      )
    )

  let outVals = zipWith (\outB cB -> outB {bScratch = bScratch cB, bConst = bConst cB}) outBinds finalCarries
      stFinal = insertOutputs (map fst outs) outVals stAfterLoop
  Right (stFinal, loopBundles, ())

lowerForIters
  :: LowerCtx kIn kOut
  -> BuildState
  -> (Id, Ty)
  -> Region kIn
  -> [Int]
  -> [Binding]
  -> Either String (BuildState, [Bundle kOut], [Binding])
lowerForIters _ st _ _ [] carries = Right (st, [], carries)
lowerForIters ctx st (ivId, ivTy) body (i : is) carries = do
  (st0, constBundles, ivScratch) <- ensureConstScratch st (fromIntegral i)
  let iv = Binding ivTy ivScratch (Just (fromIntegral i))
  (st1, bodyBundles, ys) <- lowerRegion ctx st0 body (iv : carries)
  when
    (length ys /= length carries)
    ( Left $
        "For body yield arity mismatch: expected "
          ++ show (length carries)
          ++ ", got "
          ++ show (length ys)
    )
  (st2, restBundles, finalCarries) <- lowerForIters ctx st1 (ivId, ivTy) body is ys
  Right (st2, constBundles ++ bodyBundles ++ restBundles, finalCarries)

forTripValues :: Int -> Int -> Int -> [Int]
forTripValues lb ub step
  | step > 0 = takeWhile (< ub) [lb, lb + step ..]
  | step < 0 = takeWhile (> ub) [lb, lb + step ..]
  | otherwise = []

validateExec :: Exec -> Either String ()
validateExec ex = case ex of
  ExecScalar -> Right ()
  ExecSimd w -> ensureSupportedWidth "For exec width" w

lowerAddr
  :: LowerCtx kIn kOut
  -> BuildState
  -> Addr
  -> Either String (BuildState, [Bundle kOut], ScratchAddr, Maybe Word32)
lowerAddr ctx st (Addr baseId idx) = do
  base <- lookupBinding (bsEnv st) "address base" baseId
  ensurePtrLikeTy "address base" (bTy base)
  (st1, idxBundles, idxScratch, idxConst) <- lowerIndex ctx st idx
  case idxConst of
    Just 0 -> Right (st1, idxBundles, bScratch base, bConst base)
    _ -> do
      (st2, tmp) <- allocTemp st1
      let addB = bundleAlu (Alu Add tmp (bScratch base) idxScratch)
      Right (st2, idxBundles ++ [addB], tmp, (+) <$> bConst base <*> idxConst)

lowerIndex
  :: LowerCtx kIn kOut
  -> BuildState
  -> Index
  -> Either String (BuildState, [Bundle kOut], ScratchAddr, Maybe Word32)
lowerIndex ctx st idx = case idx of
  IndexVal i -> do
    b <- lookupBinding (bsEnv st) "IndexVal" i
    ensureScalarValueTy "IndexVal" (bTy b)
    Right (st, [], bScratch b, bConst b)
  IndexAff ix -> lowerIx ctx st ix

lowerIx
  :: LowerCtx kIn kOut
  -> BuildState
  -> Ix
  -> Either String (BuildState, [Bundle kOut], ScratchAddr, Maybe Word32)
lowerIx ctx st ix = case ix of
  IxConst i -> do
    (st1, cb, s) <- ensureConstScratch st (fromIntegral i)
    Right (st1, cb, s, Just (fromIntegral i))

  IxVar i -> do
    b <- lookupBinding (bsEnv st) "IxVar" i
    ensureScalarValueTy "IxVar" (bTy b)
    Right (st, [], bScratch b, bConst b)

  IxLane ->
    Left "IxLane is not supported by FinalLowering yet"

  IxAdd a b -> do
    (stA, ba, sa, ca) <- lowerIx ctx st a
    (stB, bb, sb, cb) <- lowerIx ctx stA b
    case (ca, cb) of
      (Just wa, Just wb) -> do
        let w = wa + wb
        (stC, bc, sc) <- ensureConstScratch stB w
        Right (stC, ba ++ bb ++ bc, sc, Just w)
      (_, Just 0) ->
        Right (stB, ba ++ bb, sa, ca)
      (Just 0, _) ->
        Right (stB, ba ++ bb, sb, cb)
      _ -> do
        (stC, tmp) <- allocTemp stB
        let addB = bundleAlu (Alu Add tmp sa sb)
        Right (stC, ba ++ bb ++ [addB], tmp, Nothing)

  IxMul coeff x -> do
    let coeffW = fromIntegral coeff :: Word32
    (stX, bx, sx, cx) <- lowerIx ctx st x
    if coeff == 0
      then do
        (stZ, bz, sz) <- ensureConstScratch stX 0
        Right (stZ, bx ++ bz, sz, Just 0)
      else
        if coeff == 1
          then Right (stX, bx, sx, cx)
          else
            case cx of
              Just wx -> do
                let w = coeffW * wx
                (stC, bc, sc) <- ensureConstScratch stX w
                Right (stC, bx ++ bc, sc, Just w)
              Nothing -> do
                (stC, bCoeff, sCoeff) <- ensureConstScratch stX coeffW
                (stD, tmp) <- allocTemp stC
                let mulB = bundleAlu (Alu Mul tmp sCoeff sx)
                Right (stD, bx ++ bCoeff ++ [mulB], tmp, Nothing)

emitCopyValue
  :: LowerCtx kIn kOut
  -> BuildState
  -> Binding
  -> Binding
  -> Either String (BuildState, [Bundle kOut], ())
emitCopyValue _ st src _dst
  | isMemTy (bTy src) = Right (st, [], ())
emitCopyValue _ st src dst = do
  when
    (bTy src /= bTy dst)
    ( Left $
        "Copy type mismatch: source is "
          ++ show (bTy src)
          ++ ", destination is "
          ++ show (bTy dst)
    )
  lanes <- tySlots (bTy src)
  if bScratch src == bScratch dst || lanes <= 0
    then Right (st, [], ())
    else do
      (st1, zeroBundles, zeroS) <- ensureConstScratch st 0
      let laneCopy i =
            bundleAlu
              ( Alu
                  Add
                  (scratchPlus (bScratch dst) i)
                  (scratchPlus (bScratch src) i)
                  zeroS
              )
          copies = [laneCopy i | i <- [0 .. lanes - 1]]
      Right (st1, zeroBundles ++ copies, ())

allocTemp :: BuildState -> Either String (BuildState, ScratchAddr)
allocTemp st =
  let s = ScratchAddr (bsNextTemp st)
   in Right (st {bsNextTemp = bsNextTemp st + 1}, s)

ensureConstScratch :: BuildState -> Word32 -> Either String (BuildState, [Bundle kOut], ScratchAddr)
ensureConstScratch st w =
  case M.lookup w (bsConstTemps st) of
    Just s -> Right (st, [], s)
    Nothing -> do
      (st1, s) <- allocTemp st
      let st2 = st1 {bsConstTemps = M.insert w s (bsConstTemps st1)}
      Right (st2, [bundleLoad (Const s w)], s)

bindingForId :: LowerCtx kIn kOut -> Id -> Ty -> Maybe Word32 -> Either String Binding
bindingForId ctx i ty c = do
  case M.lookup i (lcIdTy ctx) of
    Nothing -> Left $ "Unknown id in declaration map: " ++ show i
    Just ty0 ->
      when
        (ty0 /= ty)
        ( Left $
            "Type mismatch for "
              ++ show i
              ++ ": expected "
              ++ show ty0
              ++ ", got "
              ++ show ty
        )
  let s = requireIdScratch ctx i
  Right (Binding ty s c)

requireIdScratch :: LowerCtx kIn kOut -> Id -> ScratchAddr
requireIdScratch ctx i =
  case M.lookup i (lcIdScratch ctx) of
    Just s -> s
    Nothing -> error ("Internal error: missing scratch assignment for id " ++ show i)

lookupBinding :: Map Id Binding -> String -> Id -> Either String Binding
lookupBinding env context i =
  case M.lookup i env of
    Nothing -> Left $ "Unknown id in " ++ context ++ ": " ++ show i
    Just b -> Right b

debugMapFromEnv :: Map Id Binding -> Map Id Int
debugMapFromEnv env =
  M.fromList
    [ (i, fromIntegral w)
    | (i, b) <- M.toList env
    , Just w <- [bConst b]
    ]

ensureScalarValueTy :: String -> Ty -> Either String ()
ensureScalarValueTy ctx ty =
  when
    (not (isScalarValueTy ty))
    (Left $ ctx ++ " expects scalar value type, got " ++ show ty)

ensureScalarCondTy :: String -> Ty -> Either String ()
ensureScalarCondTy ctx ty =
  when
    (not (isScalarValueTy ty))
    (Left $ ctx ++ " expects scalar condition type, got " ++ show ty)

validateVectorCondTy :: String -> Ty -> Ty -> Either String ()
validateVectorCondTy ctx condTy outTy = do
  (outW, _) <- vectorLikeWidth (ctx ++ " output") outTy
  case condTy of
    Mask w | w == outW -> Right ()
    Vec w _ | w == outW -> Right ()
    _ ->
      Left $
        ctx
          ++ " expects vector/mask condition with width "
          ++ show outW
          ++ ", got "
          ++ show condTy

ensureMemTy :: String -> Ty -> Either String ()
ensureMemTy ctx ty =
  when
    (not (isMemTy ty))
    (Left $ ctx ++ " expects Mem token, got " ++ show ty)

ensurePtrLikeTy :: String -> Ty -> Either String ()
ensurePtrLikeTy ctx ty =
  case ty of
    Ptr -> Right ()
    I32 -> Right ()
    _ -> Left $ ctx ++ " expects Ptr/I32, got " ++ show ty

expectVecTy :: String -> Ty -> Either String (Width, Ty)
expectVecTy _ (Vec w el) = Right (w, el)
expectVecTy ctx ty = Left $ ctx ++ " expects Vec type, got " ++ show ty

vectorLikeWidth :: String -> Ty -> Either String (Width, Maybe Ty)
vectorLikeWidth _ (Vec w el) = Right (w, Just el)
vectorLikeWidth _ (Mask w) = Right (w, Nothing)
vectorLikeWidth ctx ty = Left $ ctx ++ " expects vector-like type, got " ++ show ty

ensureSupportedWidth :: String -> Width -> Either String ()
ensureSupportedWidth ctx w =
  when
    (widthToInt w /= 8)
    ( Left $
        ctx
          ++ " currently supports width 8 only, got "
          ++ show (widthToInt w)
    )

isScalarValueTy :: Ty -> Bool
isScalarValueTy ty = case ty of
  I32 -> True
  Ptr -> True
  _ -> False

isVectorLikeTy :: Ty -> Bool
isVectorLikeTy ty = case ty of
  Vec _ _ -> True
  Mask _ -> True
  _ -> False

isMemTy :: Ty -> Bool
isMemTy ty = case ty of
  Mem _ -> True
  _ -> False

validateBinTypes :: AluOp -> Ty -> Ty -> Ty -> Either String ()
validateBinTypes op outTy aTy rhsTy =
  case op of
    Lt -> do
      when
        (aTy /= rhsTy)
        ( Left $
            "RBin comparison operand mismatch: lhs is "
              ++ show aTy
              ++ ", rhs is "
              ++ show rhsTy
        )
      validateCmpOut outTy aTy
    Eq_ -> do
      when
        (aTy /= rhsTy)
        ( Left $
            "RBin comparison operand mismatch: lhs is "
              ++ show aTy
              ++ ", rhs is "
              ++ show rhsTy
        )
      validateCmpOut outTy aTy
    Add ->
      validateArith outTy aTy rhsTy
    Sub ->
      validateArith outTy aTy rhsTy
    _ -> do
      when
        (aTy /= rhsTy)
        ( Left $
            "RBin operand type mismatch: lhs is "
              ++ show aTy
              ++ ", rhs is "
              ++ show rhsTy
        )
      when
        (outTy /= aTy)
        ( Left $
            "RBin output type mismatch for "
              ++ show op
              ++ ": output is "
              ++ show outTy
              ++ ", operands are "
              ++ show aTy
        )
      when (isMemTy outTy) (Left "RBin cannot produce Mem type")

validateArith :: Ty -> Ty -> Ty -> Either String ()
validateArith outTy aTy rhsTy
  | outTy == Ptr && ((aTy == Ptr && rhsTy == I32) || (aTy == I32 && rhsTy == Ptr)) = Right ()
  | otherwise = do
      when
        (aTy /= rhsTy)
        ( Left $
            "RBin operand type mismatch: lhs is "
              ++ show aTy
              ++ ", rhs is "
              ++ show rhsTy
        )
      when
        (outTy /= aTy)
        ( Left $
            "RBin output type mismatch: output is "
              ++ show outTy
              ++ ", operands are "
              ++ show aTy
        )
      when (isMemTy outTy) (Left "RBin cannot produce Mem type")

validateCmpOut :: Ty -> Ty -> Either String ()
validateCmpOut outTy inTy =
  case (outTy, inTy) of
    (I32, I32) -> Right ()
    (I32, Ptr) -> Right ()
    (Mask wOut, Vec wIn _) | wOut == wIn -> Right ()
    (Vec wOut _, Vec wIn _) | wOut == wIn -> Right ()
    (Mask wOut, Mask wIn) | wOut == wIn -> Right ()
    _ ->
      Left $
        "Comparison output/input mismatch: output is "
          ++ show outTy
          ++ ", input is "
          ++ show inTy

evalBinConst :: AluOp -> Maybe Word32 -> Maybe Word32 -> Maybe Word32
evalBinConst _ Nothing _ = Nothing
evalBinConst _ _ Nothing = Nothing
evalBinConst op (Just a) (Just b) =
  case op of
    Add -> Just (a + b)
    Sub -> Just (a - b)
    Mul -> Just (a * b)
    Div -> if b == 0 then Nothing else Just (a `div` b)
    Cdiv ->
      if b == 0
        then Nothing
        else
          let q = a `div` b
              r = a `mod` b
           in Just (if r == 0 then q else q + 1)
    Xor -> Just (xor a b)
    And -> Just (a .&. b)
    Or -> Just (a .|. b)
    Shl -> Just (shiftL a (fromIntegral b))
    Shr -> Just (shiftR a (fromIntegral b))
    Mod -> if b == 0 then Nothing else Just (a `mod` b)
    Lt -> Just (if a < b then 1 else 0)
    Eq_ -> Just (if a == b then 1 else 0)

scratchPlus :: ScratchAddr -> Int -> ScratchAddr
scratchPlus (ScratchAddr base) off = ScratchAddr (base + off)

collectKernelTypes :: Kernel k -> Either String (Map Id Ty)
collectKernelTypes kernel = do
  m0 <- foldM insertTy M.empty (kernelParams kernel)
  collectRegionTypes m0 (kernelBody kernel)

collectRegionTypes :: Map Id Ty -> Region k -> Either String (Map Id Ty)
collectRegionTypes m0 region = do
  m1 <- foldM insertTy m0 (regionParams region)
  foldM collectStmtTypes m1 (regionStmts region)

collectStmtTypes :: Map Id Ty -> Stmt k -> Either String (Map Id Ty)
collectStmtTypes m stmt = case stmt of
  Let outs _ -> foldM insertTy m outs
  Eff _ -> Right m
  If _ t e outs -> do
    m1 <- foldM insertTy m outs
    m2 <- collectRegionTypes m1 t
    collectRegionTypes m2 e
  For _ _ _ _ _ body outs -> do
    m1 <- foldM insertTy m outs
    collectRegionTypes m1 body

insertTy :: Map Id Ty -> (Id, Ty) -> Either String (Map Id Ty)
insertTy m (i, ty) =
  case M.lookup i m of
    Nothing -> Right (M.insert i ty m)
    Just oldTy ->
      if oldTy == ty
        then Right m
        else
          Left $
            "Type mismatch for "
              ++ show i
              ++ ": saw both "
              ++ show oldTy
              ++ " and "
              ++ show ty

allocateIdScratch :: Map Id Ty -> Either String (Map Id ScratchAddr)
allocateIdScratch idTy = do
  (_, outMap) <-
    foldM
      (\(next, m) (i, ty) -> do
          slots <- tySlots ty
          let base = ScratchAddr next
          Right (next + slots, M.insert i base m)
      )
      (0, M.empty)
      (sortOn fst (M.toList idTy))
  Right outMap

totalSlots :: Map Id Ty -> Either String Int
totalSlots idTy = foldM (\acc ty -> (+ acc) <$> tySlots ty) 0 (M.elems idTy)

tySlots :: Ty -> Either String Int
tySlots ty = case ty of
  I32 -> Right 1
  Ptr -> Right 1
  Mem _ -> Right 1
  Vec w _ ->
    let n = widthToInt w
     in if n > 0 then Right n else Left ("Invalid vector width: " ++ show n)
  Mask w ->
    let n = widthToInt w
     in if n > 0 then Right n else Left ("Invalid mask width: " ++ show n)

widthToInt :: Width -> Int
widthToInt (Width w) = w

bundleAlu :: AluSlot -> Bundle k
bundleAlu s = emptyBundle {aluSlots = [s]}

bundleValu :: ValuSlot -> Bundle k
bundleValu s = emptyBundle {valuSlots = [s]}

bundleLoad :: LoadSlot -> Bundle k
bundleLoad s = emptyBundle {loadSlots = [s]}

bundleStore :: StoreSlot -> Bundle k
bundleStore s = emptyBundle {storeSlots = [s]}

bundleFlow :: FlowSlot -> Bundle k
bundleFlow s = emptyBundle {flowSlots = [s]}

bundleDebug :: DebugSlot k -> Bundle k
bundleDebug s = emptyBundle {debugSlots = [s]}

emptyBundle :: Bundle k
emptyBundle = Bundle [] [] [] [] [] []
