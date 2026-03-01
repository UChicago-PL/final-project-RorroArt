{-# LANGUAGE RecordWildCards #-}

module AutoVectorizeFinalIR
  ( AutovecStats(..)
  , canonicalizeAddresses
  , autovecBaselineKernel
  ) where

import BaselineKernelFinalIR (BaselineDebugKeyRef(..), BaselineDebugRef(..))
import Control.Monad (foldM, when)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
import FinalIRUtils (collectKernelTypes, isMemTy)
import ISA (AluOp(..))

data AutovecStats = AutovecStats
  { loopsVectorized :: !Int
  , vectorLoads :: !Int
  , gatherLoads :: !Int
  , vectorStores :: !Int
  , vectorDebugCompares :: !Int
  } deriving (Show, Eq)

emptyStats :: AutovecStats
emptyStats = AutovecStats 0 0 0 0 0

addStats :: (AutovecStats -> AutovecStats) -> AVM ()
addStats f = modify' $ \s -> s { avStats = f (avStats s) }

data AVState = AVState
  { avNextId :: !Int
  , avStats :: !AutovecStats
  }

type AVM = StateT AVState (Either String)

type TyEnv = M.Map Id Ty

type ConstEnv = M.Map Id Int

data Mode = MUniform | MLaneBase | MVec
  deriving (Show, Eq)

data LaneForm
  = LFUnknown
  | LFUniform
  | LFLane !Int
  deriving (Show, Eq)

data VInfo = VInfo
  { viId :: !Id
  , viTy :: !Ty
  , viMode :: !Mode
  } deriving (Show, Eq)

data VecCtx = VecCtx
  { vcEnv :: !(M.Map Id VInfo)
  , vcBroadcasts :: !(M.Map Id Id)
  , vcOldIv :: !Id
  , vcNewIv :: !Id
  , vcLaneIds :: ![Id]
  , vcWidth :: !Width
  , vcTyEnv :: !TyEnv
  }

type VecM = StateT VecCtx AVM

vecWidth :: Width
vecWidth = Width 8

autovecBaselineKernel :: Kernel BaselineDebugKeyRef -> Either String (Kernel BaselineDebugKeyRef, AutovecStats)
autovecBaselineKernel kernel0 = do
  let kernel1 = canonicalizeAddresses kernel0
  tyEnv <- collectKernelTypes kernel1
  let start = AVState { avNextId = maxIdKernel kernel1 + 1, avStats = emptyStats }
  (body', st) <- runStateT (vectorizeRegion tyEnv M.empty (kernelBody kernel1)) start
  pure (kernel1 { kernelBody = body' }, avStats st)

freshId :: AVM Id
freshId = do
  st <- get
  let n = avNextId st
  put st { avNextId = n + 1 }
  pure (Id n)

throwErr :: String -> VecM a
throwErr msg = lift (lift (Left msg))

addStatsVec :: (AutovecStats -> AutovecStats) -> VecM ()
addStatsVec f = lift (addStats f)

--------------------------------------------------------------------------------
-- Address canonicalization prepass
--------------------------------------------------------------------------------

canonicalizeAddresses :: Kernel k -> Kernel k
canonicalizeAddresses k =
  k { kernelBody = canonicalizeRegion (kernelBody k) }

canonicalizeRegion :: Region k -> Region k
canonicalizeRegion region =
  region { regionStmts = dropDeadPtrAdds rewritten }
  where
    innerDone = map canonicalizeStmt (regionStmts region)
    ptrAdds = collectPtrAddDefs innerDone
    rewritten = map (rewriteAddrStmt ptrAdds) innerDone

canonicalizeStmt :: Stmt k -> Stmt k
canonicalizeStmt stmt = case stmt of
  If c t e outs ->
    If c (canonicalizeRegion t) (canonicalizeRegion e) outs
  For ex lb ub step inits body outs ->
    For ex lb ub step inits (canonicalizeRegion body) outs
  _ -> stmt

collectPtrAddDefs :: [Stmt k] -> M.Map Id (Id, Id)
collectPtrAddDefs = foldl step M.empty
  where
    step acc stmt = case stmt of
      Let [(out, Ptr)] (RBin Add base idx) -> M.insert out (base, idx) acc
      _ -> acc

rewriteAddrStmt :: M.Map Id (Id, Id) -> Stmt k -> Stmt k
rewriteAddrStmt ptrAdds stmt = case stmt of
  Let outs (RLoad mem addr) ->
    Let outs (RLoad mem (rewriteAddrCanon addr))
  Let outs (RStore mem addr val) ->
    Let outs (RStore mem (rewriteAddrCanon addr) val)
  If c t e outs ->
    If c t e outs
  For ex lb ub step inits body outs ->
    For ex lb ub step inits body outs
  _ -> stmt
  where
    rewriteAddrCanon (Addr base (IndexAff (IxConst 0))) =
      case M.lookup base ptrAdds of
        Just (origBase, idx) -> Addr origBase (IndexVal idx)
        Nothing -> Addr base (IndexAff (IxConst 0))
    rewriteAddrCanon x = x

dropDeadPtrAdds :: [Stmt k] -> [Stmt k]
dropDeadPtrAdds stmts = filter keep stmts
  where
    uses = collectUses stmts
    keep stmt = case stmt of
      Let [(out, Ptr)] (RBin Add _ _) -> S.member out uses
      _ -> True

--------------------------------------------------------------------------------
-- IR identifier extraction
--
-- Leaf-node extractors (rhsUses, effUses, …) are shared; the two higher-level
-- families differ only in whether definition sites are included:
--   collectUses / stmtUses / regionUses  — referenced IDs only (for DCE)
--   allIdsKernel / allIdsRegion / allIdsStmt — all IDs (for fresh-name generation)
--------------------------------------------------------------------------------

rhsUses :: Rhs -> [Id]
rhsUses rhs = case rhs of
  RConst _           -> []
  RBin _ a b         -> [a, b]
  RSelect c a b      -> [c, a, b]
  RCoreId            -> []
  RBroadcast _ s     -> [s]
  RMultiplyAdd a b c -> [a, b, c]
  RLoad m addr       -> m : addrUses addr
  RStore m addr v    -> m : v : addrUses addr

effUses :: Effect k -> [Id]
effUses eff = case eff of
  EPause             -> []
  ETraceWrite v      -> [v]
  EDebugCompare v _  -> [v]
  EDebugCompareV v _ -> [v]
  EDebugComment _    -> []
  EHalt              -> []

addrUses :: Addr -> [Id]
addrUses (Addr b idx) = b : indexUses idx

indexUses :: Index -> [Id]
indexUses idx = case idx of
  IndexVal v -> [v]
  IndexAff ix -> ixUses ix

ixUses :: Ix -> [Id]
ixUses ix = case ix of
  IxConst _ -> []
  IxVar v   -> [v]
  IxLane    -> []
  IxAdd a b -> ixUses a ++ ixUses b
  IxMul _ x -> ixUses x

-- Uses only (no definitions)

collectUses :: [Stmt k] -> S.Set Id
collectUses = foldMap stmtUses

stmtUses :: Stmt k -> S.Set Id
stmtUses stmt = case stmt of
  Let _ rhs      -> S.fromList (rhsUses rhs)
  Eff eff        -> S.fromList (effUses eff)
  If c t e _     -> S.insert c (regionUses t <> regionUses e)
  For _ _ ub _ inits body _ ->
    S.insert ub (S.fromList inits <> regionUses body)

regionUses :: Region k -> S.Set Id
regionUses Region{..} =
  foldMap stmtUses regionStmts <> S.fromList regionYield

--------------------------------------------------------------------------------
-- Loop vectorization
--------------------------------------------------------------------------------

vectorizeRegion :: TyEnv -> ConstEnv -> Region BaselineDebugKeyRef -> AVM (Region BaselineDebugKeyRef)
vectorizeRegion tyEnv constEnv region = do
  (stmts', _) <- vectorizeStmts tyEnv constEnv (regionStmts region)
  pure region { regionStmts = stmts' }

vectorizeStmts
  :: TyEnv
  -> ConstEnv
  -> [Stmt BaselineDebugKeyRef]
  -> AVM ([Stmt BaselineDebugKeyRef], ConstEnv)
vectorizeStmts _ constEnv [] = pure ([], constEnv)
vectorizeStmts tyEnv constEnv (stmt : rest) = do
  out <- vectorizeStmt tyEnv constEnv stmt
  let constEnv' = foldl updateConstEnv constEnv out
  (restOut, constEnv'') <- vectorizeStmts tyEnv constEnv' rest
  pure (out ++ restOut, constEnv'')

vectorizeStmt
  :: TyEnv
  -> ConstEnv
  -> Stmt BaselineDebugKeyRef
  -> AVM [Stmt BaselineDebugKeyRef]
vectorizeStmt tyEnv constEnv stmt = case stmt of
  If cond t e outs -> do
    t' <- vectorizeRegion tyEnv constEnv t
    e' <- vectorizeRegion tyEnv constEnv e
    pure [If cond t' e' outs]
  For ex lb ub step inits body outs -> do
    body' <- vectorizeRegion tyEnv constEnv body
    tryVectorizeLoop tyEnv constEnv (For ex lb ub step inits body' outs)
  _ -> pure [stmt]

tryVectorizeLoop
  :: TyEnv
  -> ConstEnv
  -> Stmt BaselineDebugKeyRef
  -> AVM [Stmt BaselineDebugKeyRef]
tryVectorizeLoop tyEnv constEnv stmt@(For ex lb ub step inits body outs) = do
  let canVectorize =
        ex == ExecScalar
          && step == 1
          && all isSimpleStmt (regionStmts body)
          && loopVectorizationLegal constEnv body
      maybeUb = M.lookup ub constEnv
  case (canVectorize, maybeUb) of
    (True, Just ubInt) -> do
      let trip = ubInt - lb
          vecUbInt = lb + ((max 0 trip `div` 8) * 8)
      if vecUbInt <= lb
        then pure [stmt]
        else do
          vecBody <- buildVectorBody tyEnv inits body
          addStats (\s -> s { loopsVectorized = loopsVectorized s + 1 })
          if vecUbInt == ubInt
            then do
              let vecFor =
                    For
                      { forExec = ExecSimd vecWidth
                      , forLb = lb
                      , forUb = ub
                      , forStep = 8
                      , forInits = inits
                      , forBody = vecBody
                      , forOuts = outs
                      }
              pure [vecFor]
            else do
              vecUbId <- freshId
              let vecUbConst = Let [(vecUbId, I32)] (RConst (fromIntegral vecUbInt))
              vecForOuts <- mapM (freshOut . snd) outs
              let vecFor =
                    For
                      { forExec = ExecSimd vecWidth
                      , forLb = lb
                      , forUb = vecUbId
                      , forStep = 8
                      , forInits = inits
                      , forBody = vecBody
                      , forOuts = vecForOuts
                      }
                  tailFor =
                    For
                      { forExec = ExecScalar
                      , forLb = vecUbInt
                      , forUb = ub
                      , forStep = 1
                      , forInits = map fst vecForOuts
                      , forBody = body
                      , forOuts = outs
                      }
              pure [vecUbConst, vecFor, tailFor]
    _ -> pure [For ex lb ub step inits body outs]
tryVectorizeLoop _ _ stmt = pure [stmt]

loopVectorizationLegal :: ConstEnv -> Region BaselineDebugKeyRef -> Bool
loopVectorizationLegal constEnv body =
  carriesAreMem body
    && not (hasTraceWrite (regionStmts body))
    && not (hasUnsupportedAffineStride constEnv body)

carriesAreMem :: Region k -> Bool
carriesAreMem body =
  all (isMemTy . snd) (drop 1 (regionParams body))

hasTraceWrite :: [Stmt k] -> Bool
hasTraceWrite = any isTrace
  where
    isTrace stmt = case stmt of
      Eff (ETraceWrite _) -> True
      _ -> False

hasUnsupportedAffineStride :: ConstEnv -> Region k -> Bool
hasUnsupportedAffineStride outerConstEnv body =
  any addrHasUnsupportedStride addrs
  where
    stmts = regionStmts body
    ivMay = case regionParams body of
      ((iv, _) : _) -> Just iv
      _ -> Nothing
    constEnv = foldl updateConstEnv outerConstEnv stmts
    defs = collectDefs stmts

    addrs = concatMap stmtAddrs stmts

    addrHasUnsupportedStride addr =
      hasIxLaneAddr addr || case inferAddrForm constEnv defs ivMay addr of
        LFLane stride -> stride /= 1
        _ -> False

collectDefs :: [Stmt k] -> M.Map Id Rhs
collectDefs = foldl insertStmt M.empty
  where
    insertStmt acc stmt = case stmt of
      Let [(out, _)] rhs -> M.insert out rhs acc
      _ -> acc

stmtAddrs :: Stmt k -> [Addr]
stmtAddrs stmt = case stmt of
  Let _ rhs -> rhsAddrs rhs
  _ -> []

rhsAddrs :: Rhs -> [Addr]
rhsAddrs rhs = case rhs of
  RLoad _ addr -> [addr]
  RStore _ addr _ -> [addr]
  _ -> []

hasIxLaneAddr :: Addr -> Bool
hasIxLaneAddr (Addr _ idx) = case idx of
  IndexAff ix -> ixHasLane ix
  _ -> False

ixHasLane :: Ix -> Bool
ixHasLane ix = case ix of
  IxConst _ -> False
  IxVar _ -> False
  IxLane -> True
  IxAdd a b -> ixHasLane a || ixHasLane b
  IxMul _ x -> ixHasLane x

inferAddrForm
  :: ConstEnv
  -> M.Map Id Rhs
  -> Maybe Id
  -> Addr
  -> LaneForm
inferAddrForm constEnv defs ivMay (Addr _ idx) = case idx of
  IndexVal idxId -> inferIdForm constEnv defs ivMay S.empty idxId
  IndexAff ix -> inferIxForm constEnv defs ivMay S.empty ix

inferIxForm
  :: ConstEnv
  -> M.Map Id Rhs
  -> Maybe Id
  -> S.Set Id
  -> Ix
  -> LaneForm
inferIxForm constEnv defs ivMay seen ix = case ix of
  IxConst _ -> LFUniform
  IxVar i -> inferIdForm constEnv defs ivMay seen i
  IxLane -> LFLane 1
  IxAdd a b -> combineAdd (inferIxForm constEnv defs ivMay seen a) (inferIxForm constEnv defs ivMay seen b)
  IxMul k x -> scaleLane k (inferIxForm constEnv defs ivMay seen x)

inferIdForm
  :: ConstEnv
  -> M.Map Id Rhs
  -> Maybe Id
  -> S.Set Id
  -> Id
  -> LaneForm
inferIdForm constEnv defs ivMay seen i
  | Just i == ivMay = LFLane 1
  | M.member i constEnv = LFUniform
  | S.member i seen = LFUnknown
  | otherwise =
      case M.lookup i defs of
        Nothing -> LFUnknown
        Just rhs -> inferRhsForm constEnv defs ivMay (S.insert i seen) rhs

inferRhsForm
  :: ConstEnv
  -> M.Map Id Rhs
  -> Maybe Id
  -> S.Set Id
  -> Rhs
  -> LaneForm
inferRhsForm constEnv defs ivMay seen rhs = case rhs of
  RConst _ -> LFUniform
  RBin op a b ->
    let fa = inferIdForm constEnv defs ivMay seen a
        fb = inferIdForm constEnv defs ivMay seen b
        ca = M.lookup a constEnv
        cb = M.lookup b constEnv
     in inferBinForm op fa fb ca cb
  RSelect _ t f ->
    let ft = inferIdForm constEnv defs ivMay seen t
        ff = inferIdForm constEnv defs ivMay seen f
     in if ft == ff then ft else LFUnknown
  _ -> LFUnknown

inferBinForm
  :: AluOp
  -> LaneForm
  -> LaneForm
  -> Maybe Int
  -> Maybe Int
  -> LaneForm
inferBinForm op fa fb constA constB = case op of
  Add -> combineAdd fa fb
  Sub -> combineSub fa fb
  Mul ->
    case (fa, fb, constA, constB) of
      (LFLane s, LFUniform, _, Just c) -> LFLane (s * c)
      (LFUniform, LFLane s, Just c, _) -> LFLane (s * c)
      (LFUniform, LFUniform, _, _) -> LFUniform
      _ -> LFUnknown
  _ -> LFUnknown

combineAdd :: LaneForm -> LaneForm -> LaneForm
combineAdd fa fb = case (fa, fb) of
  (LFUniform, x) -> x
  (x, LFUniform) -> x
  (LFLane a, LFLane b) -> LFLane (a + b)
  _ -> LFUnknown

combineSub :: LaneForm -> LaneForm -> LaneForm
combineSub fa fb = case (fa, fb) of
  (x, LFUniform) -> x
  (LFUniform, LFLane b) -> LFLane (negate b)
  (LFLane a, LFLane b) -> LFLane (a - b)
  _ -> LFUnknown

scaleLane :: Int -> LaneForm -> LaneForm
scaleLane k form = case form of
  LFUniform -> LFUniform
  LFLane s -> LFLane (k * s)
  LFUnknown -> LFUnknown

isSimpleStmt :: Stmt k -> Bool
isSimpleStmt s = case s of
  Let {} -> True
  Eff {} -> True
  _      -> False

freshOut :: Ty -> AVM (Id, Ty)
freshOut ty = do
  i <- freshId
  pure (i, ty)

buildVectorBody
  :: TyEnv
  -> [Id]
  -> Region BaselineDebugKeyRef
  -> AVM (Region BaselineDebugKeyRef)
buildVectorBody tyEnv _inits body = do
  (oldIv, carryParams) <- case regionParams body of
    [] -> lift (Left "vectorize: loop body missing induction parameter")
    (iv, _) : rest -> pure (iv, rest)

  newIv <- freshId
  newCarryParams <- mapM (\(_, ty) -> freshOut ty) carryParams

  (lanePrelude, laneIds) <- buildLaneIds newIv

  let startEnv =
        M.fromList
          ( (oldIv, VInfo newIv I32 MLaneBase)
              : zipWith
                  (\(oldId, ty) (newId, _) -> (oldId, VInfo newId ty MUniform))
                  carryParams
                  newCarryParams
          )
      ctx0 =
        VecCtx
          { vcEnv = startEnv
          , vcBroadcasts = M.empty
          , vcOldIv = oldIv
          , vcNewIv = newIv
          , vcLaneIds = laneIds
          , vcWidth = vecWidth
          , vcTyEnv = tyEnv
          }

  (vecStmts, ctxFinal) <- runStateT (transformStmts (regionStmts body)) ctx0

  newYields <- mapM (mappedId (vcEnv ctxFinal) (vcTyEnv ctxFinal)) (regionYield body)
  let vecBody =
        Region
          { regionParams = (newIv, I32) : newCarryParams
          , regionStmts = lanePrelude ++ vecStmts
          , regionYield = newYields
          }
  pure vecBody

buildLaneIds :: Id -> AVM ([Stmt BaselineDebugKeyRef], [Id])
buildLaneIds ivBase = do
  let lane0 = ivBase
  one <- freshId
  (stmts, lanesRev) <- foldM (step one) ([Let [(one, I32)] (RConst 1)], [lane0]) [1 .. 7 :: Int]
  pure (stmts, reverse lanesRev)
  where
    step oneId (acc, lanes@(prev : _)) _lane = do
      lid <- freshId
      let lStmt = Let [(lid, I32)] (RBin Add prev oneId)
      pure (acc ++ [lStmt], lid : lanes)
    step _ _ _ = lift (Left "vectorize: lane generation invariant broken")

transformStmts :: [Stmt BaselineDebugKeyRef] -> VecM [Stmt BaselineDebugKeyRef]
transformStmts [] = pure []
transformStmts (s : ss) = do
  out <- transformStmt s
  rest <- transformStmts ss
  pure (out ++ rest)

transformStmt :: Stmt BaselineDebugKeyRef -> VecM [Stmt BaselineDebugKeyRef]
transformStmt stmt = case stmt of
  Let outs rhs -> transformLet outs rhs
  Eff eff -> transformEff eff
  If {} -> throwErr "vectorize: nested If not supported in widened loop body"
  For {} -> throwErr "vectorize: nested For not supported in widened loop body"

transformEff :: Effect BaselineDebugKeyRef -> VecM [Stmt BaselineDebugKeyRef]
transformEff eff = case eff of
  EPause -> pure [Eff EPause]
  ETraceWrite v -> do
    info <- infoFor v
    pure [Eff (ETraceWrite (viId info))]
  EDebugCompare v k -> do
    info <- infoFor v
    oldIv <- gets vcOldIv
    newIv <- gets vcNewIv
    lanes <- gets vcLaneIds
    case viMode info of
      MVec -> do
        addStatsVec (\s -> s { vectorDebugCompares = vectorDebugCompares s + 1 })
        let ks = map (substKey oldIv) lanes
        pure [Eff (EDebugCompareV (viId info) (map (\f -> f k) ks))]
      _ -> do
        let k' = substKey oldIv newIv k
        pure [Eff (EDebugCompare (viId info) k')]
  EDebugCompareV v ks -> do
    info <- infoFor v
    pure [Eff (EDebugCompareV (viId info) ks)]
  EDebugComment msg -> pure [Eff (EDebugComment msg)]
  EHalt -> pure [Eff EHalt]

transformLet :: [(Id, Ty)] -> Rhs -> VecM [Stmt BaselineDebugKeyRef]
transformLet outs rhs = case rhs of
  RLoad mem addr -> transformLoad outs mem addr
  RStore mem addr val -> transformStore outs mem addr val
  RBin op a b -> transformBin outs op a b
  RSelect c a b -> transformSelect outs c a b
  RConst w -> case outs of
    [(oldOut, outTy)] -> do
      newOut <- freshVecId
      setInfo oldOut (VInfo newOut outTy MUniform)
      pure [Let [(newOut, outTy)] (RConst w)]
    _ -> throwErr "vectorize: RConst expects 1 output"
  RCoreId -> case outs of
    [(oldOut, outTy)] -> do
      newOut <- freshVecId
      setInfo oldOut (VInfo newOut outTy MUniform)
      pure [Let [(newOut, outTy)] RCoreId]
    _ -> throwErr "vectorize: RCoreId expects 1 output"
  RBroadcast w src -> case outs of
    [(oldOut, outTy)] -> do
      srcI <- infoFor src
      newOut <- freshVecId
      setInfo oldOut (VInfo newOut outTy MVec)
      pure [Let [(newOut, outTy)] (RBroadcast w (viId srcI))]
    _ -> throwErr "vectorize: RBroadcast expects 1 output"
  RMultiplyAdd a b c -> case outs of
    [(oldOut, outTy)] -> do
      ai <- infoFor a
      bi <- infoFor b
      ci <- infoFor c
      newOut <- freshVecId
      setInfo oldOut (VInfo newOut outTy MVec)
      pure [Let [(newOut, outTy)] (RMultiplyAdd (viId ai) (viId bi) (viId ci))]
    _ -> throwErr "vectorize: RMultiplyAdd expects 1 output"

transformLoad :: [(Id, Ty)] -> Id -> Addr -> VecM [Stmt BaselineDebugKeyRef]
transformLoad outs mem addr = case outs of
  [(oldVal, oldValTy)] ->
    doLoad oldVal oldValTy Nothing
  [(oldVal, oldValTy), (oldMem, oldMemTy)] ->
    doLoad oldVal oldValTy (Just (oldMem, oldMemTy))
  _ -> throwErr "vectorize: RLoad output arity unsupported"
  where
    doLoad oldVal oldValTy memOutInfo = do
      memI <- infoFor mem
      (addr', idxMode) <- rewriteAddr addr
      let vecValTy = Vec vecWidth oldValTy
          shouldVec = oldValTy == I32 && (idxMode == MLaneBase || idxMode == MVec)
          newValTy = if shouldVec then vecValTy else oldValTy
          newMode = if shouldVec then MVec else MUniform
      newVal <- freshVecId
      memOuts <- case memOutInfo of
        Just (oldMem, oldMemTy) -> do
          mOut <- freshVecId
          setInfo oldMem (VInfo mOut oldMemTy MUniform)
          pure [(mOut, oldMemTy)]
        Nothing ->
          pure []
      let stmt' = Let ((newVal, newValTy) : memOuts) (RLoad (viId memI) addr')
      setInfo oldVal (VInfo newVal newValTy newMode)
      when shouldVec $
        case idxMode of
          MLaneBase -> addStatsVec (\s -> s { vectorLoads = vectorLoads s + 1 })
          MVec -> addStatsVec (\s -> s { gatherLoads = gatherLoads s + 1 })
          _ -> pure ()
      pure [stmt']

transformStore :: [(Id, Ty)] -> Id -> Addr -> Id -> VecM [Stmt BaselineDebugKeyRef]
transformStore outs mem addr val = case outs of
  [] -> doStore Nothing
  [(oldMem, oldMemTy)] -> doStore (Just (oldMem, oldMemTy))
  _ -> throwErr "vectorize: RStore output arity unsupported"
  where
    doStore memOutInfo = do
      memI <- infoFor mem
      valI <- infoFor val
      (addr', idxMode) <- rewriteAddr addr
      memOuts <- case memOutInfo of
        Just (oldMem, oldMemTy) -> do
          mOut <- freshVecId
          setInfo oldMem (VInfo mOut oldMemTy MUniform)
          pure [(mOut, oldMemTy)]
        Nothing ->
          pure []
      let stmt' = Let memOuts (RStore (viId memI) addr' (viId valI))
      when (viMode valI == MVec && (idxMode == MLaneBase || idxMode == MVec)) $
        addStatsVec (\s -> s { vectorStores = vectorStores s + 1 })
      pure [stmt']

transformBin :: [(Id, Ty)] -> AluOp -> Id -> Id -> VecM [Stmt BaselineDebugKeyRef]
transformBin outs op a b = case outs of
  [(oldOut, outTy)] -> do
    ai <- infoFor a
    bi <- infoFor b
    let shouldVec = outTy == I32 && (viMode ai == MVec || viMode bi == MVec)
    if shouldVec
      then do
        (aVec, preA) <- ensureVec ai
        (bVec, preB) <- ensureVec bi
        out <- freshVecId
        let outTy' = Vec vecWidth I32
        setInfo oldOut (VInfo out outTy' MVec)
        pure (preA ++ preB ++ [Let [(out, outTy')] (RBin op aVec bVec)])
      else do
        out <- freshVecId
        let modeOut =
              if outTy == I32 && (viMode ai == MLaneBase || viMode bi == MLaneBase)
                then MLaneBase
                else MUniform
        setInfo oldOut (VInfo out outTy modeOut)
        pure [Let [(out, outTy)] (RBin op (viId ai) (viId bi))]
  _ -> throwErr "vectorize: RBin expects 1 output"

transformSelect :: [(Id, Ty)] -> Id -> Id -> Id -> VecM [Stmt BaselineDebugKeyRef]
transformSelect outs c a b = case outs of
  [(oldOut, outTy)] -> do
    ci <- infoFor c
    ai <- infoFor a
    bi <- infoFor b
    let shouldVec = outTy == I32 && any (== MVec) [viMode ci, viMode ai, viMode bi]
    if shouldVec
      then do
        (cVec, preC) <- ensureVec ci
        (aVec, preA) <- ensureVec ai
        (bVec, preB) <- ensureVec bi
        out <- freshVecId
        let outTy' = Vec vecWidth I32
        setInfo oldOut (VInfo out outTy' MVec)
        pure (preC ++ preA ++ preB ++ [Let [(out, outTy')] (RSelect cVec aVec bVec)])
      else do
        out <- freshVecId
        let modeOut =
              if outTy == I32 && any (== MLaneBase) [viMode ci, viMode ai, viMode bi]
                then MLaneBase
                else MUniform
        setInfo oldOut (VInfo out outTy modeOut)
        pure [Let [(out, outTy)] (RSelect (viId ci) (viId ai) (viId bi))]
  _ -> throwErr "vectorize: RSelect expects 1 output"

ensureVec :: VInfo -> VecM (Id, [Stmt BaselineDebugKeyRef])
ensureVec VInfo{..}
  | viMode == MVec = pure (viId, [])
ensureVec VInfo{..}
  | viTy == I32 = do
      cache <- gets vcBroadcasts
      case M.lookup viId cache of
        Just v -> pure (v, [])
        Nothing -> do
          v <- freshVecId
          w <- gets vcWidth
          modify' (\ctx -> ctx { vcBroadcasts = M.insert viId v (vcBroadcasts ctx) })
          pure (v, [Let [(v, Vec w I32)] (RBroadcast w viId)])
ensureVec VInfo{..} =
  throwErr ("vectorize: cannot broadcast non-I32 value " ++ show viId ++ " with type " ++ show viTy)

rewriteAddr :: Addr -> VecM (Addr, Mode)
rewriteAddr (Addr base idx) = do
  baseI <- infoFor base
  (idx', m) <- rewriteIndex idx
  pure (Addr (viId baseI) idx', m)

rewriteIndex :: Index -> VecM (Index, Mode)
rewriteIndex idx = case idx of
  IndexVal i -> do
    iInfo <- infoFor i
    pure (IndexVal (viId iInfo), viMode iInfo)
  IndexAff ix -> do
    ix' <- rewriteIx ix
    m <- modeOfIx ix
    pure (IndexAff ix', m)

rewriteIx :: Ix -> VecM Ix
rewriteIx ix = case ix of
  IxConst n -> pure (IxConst n)
  IxLane -> pure IxLane
  IxVar i -> do
    info <- infoFor i
    pure (IxVar (viId info))
  IxAdd a b -> IxAdd <$> rewriteIx a <*> rewriteIx b
  IxMul k x -> IxMul k <$> rewriteIx x

modeOfIx :: Ix -> VecM Mode
modeOfIx ix = case ix of
  IxConst _ -> pure MUniform
  IxLane -> pure MVec
  IxVar i -> viMode <$> infoFor i
  IxAdd a b -> combineMode <$> modeOfIx a <*> modeOfIx b
  IxMul _ x -> modeOfIx x

combineMode :: Mode -> Mode -> Mode
combineMode a b
  | a == MVec || b == MVec = MVec
  | a == MLaneBase || b == MLaneBase = MLaneBase
  | otherwise = MUniform

infoFor :: Id -> VecM VInfo
infoFor old = do
  env <- gets vcEnv
  case M.lookup old env of
    Just v -> pure v
    Nothing -> do
      tyEnv <- gets vcTyEnv
      case M.lookup old tyEnv of
        Just ty -> pure (VInfo old ty MUniform)
        Nothing -> throwErr ("vectorize: missing type for id " ++ show old)

mappedId :: M.Map Id VInfo -> TyEnv -> Id -> AVM Id
mappedId env tyEnv old =
  case M.lookup old env of
    Just v -> pure (viId v)
    Nothing ->
      case M.lookup old tyEnv of
        Just _ -> pure old
        Nothing -> lift (Left ("vectorize: missing mapped id " ++ show old))

setInfo :: Id -> VInfo -> VecM ()
setInfo old info = modify' (\ctx -> ctx { vcEnv = M.insert old info (vcEnv ctx) })

freshVecId :: VecM Id
freshVecId = lift freshId

substKey :: Id -> Id -> BaselineDebugKeyRef -> BaselineDebugKeyRef
substKey oldIv newIv key = case key of
  KeyIdx r b -> KeyIdx (substRef r) (substRef b)
  KeyVal r b -> KeyVal (substRef r) (substRef b)
  KeyNodeVal r b -> KeyNodeVal (substRef r) (substRef b)
  KeyHashStage r b s -> KeyHashStage (substRef r) (substRef b) s
  KeyHashedVal r b -> KeyHashedVal (substRef r) (substRef b)
  KeyNextIdx r b -> KeyNextIdx (substRef r) (substRef b)
  KeyWrappedIdx r b -> KeyWrappedIdx (substRef r) (substRef b)
  where
    substRef ref = case ref of
      RefConst n -> RefConst n
      RefIv i | i == oldIv -> RefIv newIv
      RefIv i -> RefIv i

--------------------------------------------------------------------------------
-- Type/constant helpers
--------------------------------------------------------------------------------

updateConstEnv :: ConstEnv -> Stmt k -> ConstEnv
updateConstEnv env stmt = case stmt of
  Let [(out, I32)] (RConst w) -> M.insert out (fromIntegral w) env
  _ -> env

-- All IDs: both definitions and uses (for fresh-name generation)

maxIdKernel :: Kernel k -> Int
maxIdKernel kernel =
  case S.lookupMax (allIdsKernel kernel) of
    Nothing     -> -1
    Just (Id n) -> n

allIdsKernel :: Kernel k -> S.Set Id
allIdsKernel Kernel{..} =
  S.fromList (map fst kernelParams) <> allIdsRegion kernelBody

allIdsRegion :: Region k -> S.Set Id
allIdsRegion Region{..} =
  S.fromList (map fst regionParams)
    <> foldMap allIdsStmt regionStmts
    <> S.fromList regionYield

allIdsStmt :: Stmt k -> S.Set Id
allIdsStmt stmt = case stmt of
  Let outs rhs ->
    S.fromList (map fst outs) <> S.fromList (rhsUses rhs)
  Eff eff ->
    S.fromList (effUses eff)
  If c t e outs ->
    S.fromList (map fst outs) <> S.insert c (allIdsRegion t <> allIdsRegion e)
  For _ _ ub _ inits body outs ->
    S.fromList (map fst outs) <> S.insert ub (S.fromList inits <> allIdsRegion body)

