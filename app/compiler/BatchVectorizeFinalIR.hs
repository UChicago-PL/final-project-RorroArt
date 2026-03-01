{-# LANGUAGE RecordWildCards #-}

module BatchVectorizeFinalIR
  ( BatchStats(..)
  , DebugPolicy(..)
  , BatchConfig(..)
  , defaultBatchConfig
  , canonicalizeAddresses
  , batchVectorizeKernel
  ) where

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
import FinalIRUtils
  ( collectKernelTypes
  , collectUses
  , isMemTy
  , maxIdKernel
  , stmtUses
  , widthToInt
  )
import ISA (AluOp(..))

data BatchStats = BatchStats
  { loopsVectorized :: !Int
  , vectorLoads :: !Int
  , gatherLoads :: !Int
  , vectorStores :: !Int
  , vectorDebugCompares :: !Int
  } deriving (Show, Eq)

data DebugPolicy k = DebugPolicy
  { rewriteScalarDebugKey :: Id -> Id -> k -> Either String k
  , expandVectorDebugKey :: Id -> [Id] -> k -> Either String [k]
  , rewriteExistingVectorDebugKeys :: Id -> [Id] -> [k] -> Either String [k]
  }

data BatchConfig k = BatchConfig
  { bcWidth :: !Width
  , bcDebugPolicy :: !(DebugPolicy k)
  }

defaultDebugPolicy :: DebugPolicy k
defaultDebugPolicy =
  DebugPolicy
    { rewriteScalarDebugKey = \_ _ k -> Right k
    , expandVectorDebugKey = \_ laneIds k -> Right (replicate (length laneIds) k)
    , rewriteExistingVectorDebugKeys = \_ _ ks -> Right ks
    }

defaultBatchConfig :: BatchConfig k
defaultBatchConfig =
  BatchConfig
    { bcWidth = Width 8
    , bcDebugPolicy = defaultDebugPolicy
    }

emptyStats :: BatchStats
emptyStats = BatchStats 0 0 0 0 0

addStats :: (BatchStats -> BatchStats) -> AVM ()
addStats f = modify' $ \s -> s { avStats = f (avStats s) }

data AVState = AVState
  { avNextId :: !Int
  , avStats :: !BatchStats
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

data VecCtx k = VecCtx
  { vcEnv :: !(M.Map Id VInfo)
  , vcBroadcasts :: !(M.Map Id Id)
  , vcReductionOuts :: !(M.Map Id ReductionSpec)
  , vcDebugPolicy :: !(DebugPolicy k)
  , vcOldIv :: !Id
  , vcNewIv :: !Id
  , vcLaneIds :: ![Id]
  , vcWidth :: !Width
  , vcTyEnv :: !TyEnv
  }

data ReductionSpec = ReductionSpec
  { rsOp :: !AluOp
  , rsCarryIn :: !Id
  } deriving (Show, Eq)

data LoopAnalysis = LoopAnalysis
  { laReductionOuts :: !(M.Map Id ReductionSpec)
  } deriving (Show, Eq)

type VecM k = StateT (VecCtx k) AVM

batchVectorizeKernel :: BatchConfig k -> Kernel k -> Either String (Kernel k, BatchStats)
batchVectorizeKernel cfg kernel0 = do
  let kernel1 = ifConvertKernel (canonicalizeAddresses kernel0)
  tyEnv <- collectKernelTypes kernel1
  let start = AVState { avNextId = maxIdKernel kernel1 + 1, avStats = emptyStats }
  (body', st) <- runStateT (vectorizeRegion cfg tyEnv M.empty (kernelBody kernel1)) start
  pure (kernel1 { kernelBody = body' }, avStats st)

freshId :: AVM Id
freshId = do
  st <- get
  let n = avNextId st
  put st { avNextId = n + 1 }
  pure (Id n)

throwErr :: String -> VecM k a
throwErr msg = lift (lift (Left msg))

addStatsVec :: (BatchStats -> BatchStats) -> VecM k ()
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
-- If-conversion prepass
--------------------------------------------------------------------------------

ifConvertKernel :: Kernel k -> Kernel k
ifConvertKernel kernel =
  kernel { kernelBody = ifConvertRegion (kernelBody kernel) }

ifConvertRegion :: Region k -> Region k
ifConvertRegion region =
  region { regionStmts = concatMap ifConvertStmt (regionStmts region) }

ifConvertStmt :: Stmt k -> [Stmt k]
ifConvertStmt stmt = case stmt of
  Let {} -> [stmt]
  Eff {} -> [stmt]
  For ex lb ub step inits body outs ->
    [For ex lb ub step inits (ifConvertRegion body) outs]
  If cond thenRegion elseRegion outs ->
    let then' = ifConvertRegion thenRegion
        else' = ifConvertRegion elseRegion
     in if canIfConvert then' else' outs
          then
            regionStmts then'
              ++ regionStmts else'
              ++ buildSelects cond outs (regionYield then') (regionYield else')
          else [If cond then' else' outs]

canIfConvert :: Region k -> Region k -> [(Id, Ty)] -> Bool
canIfConvert thenRegion elseRegion outs =
  null (regionParams thenRegion)
    && null (regionParams elseRegion)
    && all isPureLet (regionStmts thenRegion)
    && all isPureLet (regionStmts elseRegion)
    && length (regionYield thenRegion) == length outs
    && length (regionYield elseRegion) == length outs
  where
    isPureLet st = case st of
      Let _ rhs -> isPureRhs rhs
      _ -> False

    isPureRhs rhs = case rhs of
      RLoad {} -> False
      RStore {} -> False
      _ -> True

buildSelects :: Id -> [(Id, Ty)] -> [Id] -> [Id] -> [Stmt k]
buildSelects _ [] [] [] = []
buildSelects cond ((outId, outTy) : outs) (thenId : thenRest) (elseId : elseRest) =
  Let
    { letOuts = [(outId, outTy)]
    , letRhs = RSelect cond thenId elseId
    }
    : buildSelects cond outs thenRest elseRest
buildSelects _ _ _ _ = error "buildSelects: arity mismatch between outs and region yields"

--------------------------------------------------------------------------------
-- Loop vectorization
--------------------------------------------------------------------------------

vectorizeRegion :: BatchConfig k -> TyEnv -> ConstEnv -> Region k -> AVM (Region k)
vectorizeRegion cfg tyEnv constEnv region = do
  (stmts', _) <- vectorizeStmts cfg tyEnv constEnv (regionStmts region)
  pure region { regionStmts = stmts' }

vectorizeStmts
  :: BatchConfig k
  -> TyEnv
  -> ConstEnv
  -> [Stmt k]
  -> AVM ([Stmt k], ConstEnv)
vectorizeStmts _ _ constEnv [] = pure ([], constEnv)
vectorizeStmts cfg tyEnv constEnv (stmt : rest) = do
  out <- vectorizeStmt cfg tyEnv constEnv stmt
  let constEnv' = foldl updateConstEnv constEnv out
  (restOut, constEnv'') <- vectorizeStmts cfg tyEnv constEnv' rest
  pure (out ++ restOut, constEnv'')

vectorizeStmt
  :: BatchConfig k
  -> TyEnv
  -> ConstEnv
  -> Stmt k
  -> AVM [Stmt k]
vectorizeStmt cfg tyEnv constEnv stmt = case stmt of
  If cond t e outs -> do
    t' <- vectorizeRegion cfg tyEnv constEnv t
    e' <- vectorizeRegion cfg tyEnv constEnv e
    pure [If cond t' e' outs]
  For ex lb ub step inits body outs -> do
    body' <- vectorizeRegion cfg tyEnv constEnv body
    tryVectorizeLoop cfg tyEnv constEnv (For ex lb ub step inits body' outs)
  _ -> pure [stmt]

tryVectorizeLoop
  :: BatchConfig k
  -> TyEnv
  -> ConstEnv
  -> Stmt k
  -> AVM [Stmt k]
tryVectorizeLoop cfg tyEnv constEnv stmt@(For ex lb ub step inits body outs) = do
  let maybeAnalysis =
        analyzeLoopVectorization constEnv body
      widthInt = batchWidthInt cfg
      canVectorize =
        ex == ExecScalar
          && step == 1
          && all isSimpleStmt (regionStmts body)
          && widthInt > 0
          && maybeAnalysis /= Nothing
      maybeUb = M.lookup ub constEnv
  case (canVectorize, maybeUb, maybeAnalysis) of
    (True, Just ubInt, Just analysis) -> do
      let trip = ubInt - lb
          vecUbInt = lb + ((max 0 trip `div` widthInt) * widthInt)
      if vecUbInt <= lb
        then pure [stmt]
        else do
          vecBody <- buildVectorBody cfg tyEnv analysis inits body
          addStats (\s -> s { loopsVectorized = loopsVectorized s + 1 })
          if vecUbInt == ubInt
            then do
              let vecFor =
                    For
                      { forExec = ExecSimd (bcWidth cfg)
                      , forLb = lb
                      , forUb = ub
                      , forStep = widthInt
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
                      { forExec = ExecSimd (bcWidth cfg)
                      , forLb = lb
                      , forUb = vecUbId
                      , forStep = widthInt
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
tryVectorizeLoop _ _ _ stmt = pure [stmt]

analyzeLoopVectorization :: ConstEnv -> Region k -> Maybe LoopAnalysis
analyzeLoopVectorization constEnv body =
  if
      carriesLegal
    && not (hasUnsupportedEffects (regionStmts body))
    && not (hasUnsupportedAffineStride constEnv body)
    then Just (LoopAnalysis reductionOuts)
    else Nothing
  where
    (carriesLegal, reductionOuts) = analyzeCarries body

analyzeCarries :: Region k -> (Bool, M.Map Id ReductionSpec)
analyzeCarries body =
  case regionParams body of
    [] -> (False, M.empty)
    (_ivParam : carryParams) ->
      let ys = regionYield body
          carryCount = length carryParams
          defs = collectDefs (regionStmts body)
          bodyUses = foldMap stmtUses (regionStmts body)
          carryYields = take carryCount ys
          steps = zip carryParams carryYields
       in if length ys /= carryCount
            then (False, M.empty)
            else foldl (step defs bodyUses) (True, M.empty) steps
  where
    step defs bodyUses (ok, acc) ((carryId, carryTy), yieldId)
      | not ok = (False, acc)
      | isMemTy carryTy = (True, acc)
      | carryTy == I32 =
          case M.lookup yieldId defs of
            Just (RBin op a b)
              | associativeReductionOp op
              , a == carryId || b == carryId
              , not (S.member yieldId bodyUses) ->
                  (True, M.insert yieldId (ReductionSpec op carryId) acc)
            _ -> (False, acc)
      | otherwise = (False, acc)

associativeReductionOp :: AluOp -> Bool
associativeReductionOp op = case op of
  Add -> True
  Mul -> True
  Xor -> True
  And -> True
  Or -> True
  _ -> False

batchWidthInt :: BatchConfig k -> Int
batchWidthInt cfg =
  case bcWidth cfg of
    Width n -> n

hasUnsupportedEffects :: [Stmt k] -> Bool
hasUnsupportedEffects = any isUnsupported
  where
    isUnsupported stmt = case stmt of
      Eff EPause -> True
      Eff (ETraceWrite _) -> True
      Eff EHalt -> True
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
  :: BatchConfig k
  -> TyEnv
  -> LoopAnalysis
  -> [Id]
  -> Region k
  -> AVM (Region k)
buildVectorBody cfg tyEnv analysis _inits body = do
  (oldIv, carryParams) <- case regionParams body of
    [] -> lift (Left "vectorize: loop body missing induction parameter")
    (iv, _) : rest -> pure (iv, rest)

  newIv <- freshId
  newCarryParams <- mapM (\(_, ty) -> freshOut ty) carryParams

  (lanePrelude, laneIds) <- buildLaneIds (bcWidth cfg) newIv

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
          , vcReductionOuts = laReductionOuts analysis
          , vcDebugPolicy = bcDebugPolicy cfg
          , vcOldIv = oldIv
          , vcNewIv = newIv
          , vcLaneIds = laneIds
          , vcWidth = bcWidth cfg
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

buildLaneIds :: Width -> Id -> AVM ([Stmt k], [Id])
buildLaneIds w ivBase = do
  let lane0 = ivBase
      laneCount = widthToInt w
  one <- freshId
  when (laneCount <= 0) (lift (Left "vectorize: invalid SIMD width"))
  (stmts, lanesRev) <- foldM (step one) ([Let [(one, I32)] (RConst 1)], [lane0]) [1 .. laneCount - 1]
  pure (stmts, reverse lanesRev)
  where
    step oneId (acc, lanes@(prev : _)) _lane = do
      lid <- freshId
      let lStmt = Let [(lid, I32)] (RBin Add prev oneId)
      pure (acc ++ [lStmt], lid : lanes)
    step _ _ _ = lift (Left "vectorize: lane generation invariant broken")

transformStmts :: [Stmt k] -> VecM k [Stmt k]
transformStmts [] = pure []
transformStmts (s : ss) = do
  out <- transformStmt s
  rest <- transformStmts ss
  pure (out ++ rest)

transformStmt :: Stmt k -> VecM k [Stmt k]
transformStmt stmt = case stmt of
  Let outs rhs -> transformLet outs rhs
  Eff eff -> transformEff eff
  If {} -> throwErr "vectorize: nested If not supported in widened loop body"
  For {} -> throwErr "vectorize: nested For not supported in widened loop body"

transformEff :: Effect k -> VecM k [Stmt k]
transformEff eff = case eff of
  EPause -> pure [Eff EPause]
  ETraceWrite v -> do
    info <- infoFor v
    pure [Eff (ETraceWrite (viId info))]
  EDebugCompare v k -> do
    info <- infoFor v
    policy <- gets vcDebugPolicy
    oldIv <- gets vcOldIv
    newIv <- gets vcNewIv
    lanes <- gets vcLaneIds
    case viMode info of
      MVec -> do
        addStatsVec (\s -> s { vectorDebugCompares = vectorDebugCompares s + 1 })
        ks <- liftEitherVec (expandVectorDebugKey policy oldIv lanes k)
        pure [Eff (EDebugCompareV (viId info) ks)]
      _ -> do
        k' <- liftEitherVec (rewriteScalarDebugKey policy oldIv newIv k)
        pure [Eff (EDebugCompare (viId info) k')]
  EDebugCompareV v ks -> do
    info <- infoFor v
    policy <- gets vcDebugPolicy
    oldIv <- gets vcOldIv
    lanes <- gets vcLaneIds
    ks' <- liftEitherVec (rewriteExistingVectorDebugKeys policy oldIv lanes ks)
    pure [Eff (EDebugCompareV (viId info) ks')]
  EDebugComment msg -> pure [Eff (EDebugComment msg)]
  EHalt -> pure [Eff EHalt]

transformLet :: [(Id, Ty)] -> Rhs -> VecM k [Stmt k]
transformLet outs rhs = case rhs of
  RLoad mem addr -> transformLoad outs mem addr
  RStore mem addr val -> transformStore outs mem addr val
  RBin op a b -> transformBin outs op a b
  RReduce op src -> transformReduce outs op src
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

transformLoad :: [(Id, Ty)] -> Id -> Addr -> VecM k [Stmt k]
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
      width <- gets vcWidth
      let vecValTy = Vec width oldValTy
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

transformStore :: [(Id, Ty)] -> Id -> Addr -> Id -> VecM k [Stmt k]
transformStore outs mem addr val = case outs of
  [] -> doStore Nothing
  [(oldMem, oldMemTy)] -> doStore (Just (oldMem, oldMemTy))
  _ -> throwErr "vectorize: RStore output arity unsupported"
  where
    doStore memOutInfo = do
      memI <- infoFor mem
      valI <- infoFor val
      (addr', idxMode) <- rewriteAddr addr
      let needsVectorStore = idxMode == MLaneBase || idxMode == MVec
      (valOutId, preStmts, valOutMode) <-
        if needsVectorStore
          then do
            if viMode valI == MVec
              then pure (viId valI, [], MVec)
              else do
                (v, pre) <- ensureVec valI
                pure (v, pre, MVec)
          else pure (viId valI, [], viMode valI)
      memOuts <- case memOutInfo of
        Just (oldMem, oldMemTy) -> do
          mOut <- freshVecId
          setInfo oldMem (VInfo mOut oldMemTy MUniform)
          pure [(mOut, oldMemTy)]
        Nothing ->
          pure []
      let stmt' = Let memOuts (RStore (viId memI) addr' valOutId)
      when (valOutMode == MVec && needsVectorStore) $
        addStatsVec (\s -> s { vectorStores = vectorStores s + 1 })
      pure (preStmts ++ [stmt'])

transformBin :: [(Id, Ty)] -> AluOp -> Id -> Id -> VecM k [Stmt k]
transformBin outs op a b = case outs of
  [(oldOut, outTy)] -> do
    ai <- infoFor a
    bi <- infoFor b
    reductionOuts <- gets vcReductionOuts
    case M.lookup oldOut reductionOuts of
      Just spec ->
        transformReductionBin oldOut outTy op a ai b bi spec
      Nothing -> do
        let shouldVec = outTy == I32 && (viMode ai == MVec || viMode bi == MVec)
        if shouldVec
          then do
            (aVec, preA) <- ensureVec ai
            (bVec, preB) <- ensureVec bi
            out <- freshVecId
            width <- gets vcWidth
            let outTy' = Vec width I32
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

transformReduce :: [(Id, Ty)] -> AluOp -> Id -> VecM k [Stmt k]
transformReduce outs op src = case outs of
  [(oldOut, outTy)] -> do
    srcI <- infoFor src
    out <- freshVecId
    setInfo oldOut (VInfo out outTy MUniform)
    pure [Let [(out, outTy)] (RReduce op (viId srcI))]
  _ -> throwErr "vectorize: RReduce expects 1 output"

transformReductionBin
  :: Id
  -> Ty
  -> AluOp
  -> Id
  -> VInfo
  -> Id
  -> VInfo
  -> ReductionSpec
  -> VecM k [Stmt k]
transformReductionBin oldOut outTy op a ai b bi ReductionSpec{..} = do
  when (outTy /= I32) (throwErr "vectorize: reduction output must be I32")
  when (op /= rsOp) (throwErr "vectorize: reduction op mismatch in transformed body")
  (carryInfo, contribInfo) <-
    if a == rsCarryIn then pure (ai, bi)
    else if b == rsCarryIn then pure (bi, ai)
    else throwErr "vectorize: reduction carry operand missing"
  when
    (viTy carryInfo /= I32 || viMode carryInfo /= MUniform)
    (throwErr "vectorize: reduction carry must remain scalar I32")
  case viMode contribInfo of
    MVec -> do
      case viTy contribInfo of
        Vec _ I32 -> pure ()
        _ -> throwErr "vectorize: vector reduction contribution must be Vec _ I32"
      outReduce <- freshVecId
      out <- freshVecId
      setInfo oldOut (VInfo out I32 MUniform)
      pure
        [ Let [(outReduce, I32)] (RReduce op (viId contribInfo))
        , Let [(out, I32)] (RBin op (viId carryInfo) outReduce)
        ]
    MUniform -> do
      when
        (viTy contribInfo /= I32)
        (throwErr "vectorize: scalar reduction contribution must be I32")
      out <- freshVecId
      setInfo oldOut (VInfo out I32 MUniform)
      pure [Let [(out, I32)] (RBin op (viId carryInfo) (viId contribInfo))]
    MLaneBase ->
      throwErr "vectorize: lane-base reduction contribution unsupported"

transformSelect :: [(Id, Ty)] -> Id -> Id -> Id -> VecM k [Stmt k]
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
        width <- gets vcWidth
        let outTy' = Vec width I32
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

ensureVec :: VInfo -> VecM k (Id, [Stmt k])
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

rewriteAddr :: Addr -> VecM k (Addr, Mode)
rewriteAddr (Addr base idx) = do
  baseI <- infoFor base
  (idx', m) <- rewriteIndex idx
  pure (Addr (viId baseI) idx', m)

rewriteIndex :: Index -> VecM k (Index, Mode)
rewriteIndex idx = case idx of
  IndexVal i -> do
    iInfo <- infoFor i
    pure (IndexVal (viId iInfo), viMode iInfo)
  IndexAff ix -> do
    ix' <- rewriteIx ix
    m <- modeOfIx ix
    pure (IndexAff ix', m)

rewriteIx :: Ix -> VecM k Ix
rewriteIx ix = case ix of
  IxConst n -> pure (IxConst n)
  IxLane -> pure IxLane
  IxVar i -> do
    info <- infoFor i
    pure (IxVar (viId info))
  IxAdd a b -> IxAdd <$> rewriteIx a <*> rewriteIx b
  IxMul c x -> IxMul c <$> rewriteIx x

modeOfIx :: Ix -> VecM k Mode
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

infoFor :: Id -> VecM k VInfo
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

setInfo :: Id -> VInfo -> VecM k ()
setInfo old info = modify' (\ctx -> ctx { vcEnv = M.insert old info (vcEnv ctx) })

freshVecId :: VecM k Id
freshVecId = lift freshId

--------------------------------------------------------------------------------
-- Type/constant helpers
--------------------------------------------------------------------------------

liftEitherVec :: Either String a -> VecM k a
liftEitherVec = either throwErr pure

updateConstEnv :: ConstEnv -> Stmt k -> ConstEnv
updateConstEnv env stmt = case stmt of
  Let [(out, I32)] (RConst w) -> M.insert out (fromIntegral w) env
  _ -> env
