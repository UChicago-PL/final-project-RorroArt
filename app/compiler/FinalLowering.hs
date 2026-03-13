module FinalLowering
  ( lowerToMachineOps
  ) where

import Control.Monad.State.Strict (State, get, gets, put, modify, execState)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word (Word32)

import ISA (ScratchAddr(..), Offset(..), AluOp(..))
import qualified ISA
import FinalIR
import Machine

-- | Lower IR to a flat list of typed machine ops.
lowerToMachineOps :: Int -> Function -> [MachineOp]
lowerToMachineOps w fn@(Function ss) = reverse (lsOpsRev stFinal)
  where
    tys      = collectTypes fn
    loopVars = collectLoopVars fn
    headerLets = takeHeaderLets ss
    st0 = initLowerSt w tys loopVars headerLets
    stFinal = execState (lowerTop ss) st0

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

data LowerSt = LowerSt
  { lsW           :: !Int
  , lsNextScratch :: !Int
  , lsIdReg       :: Map Id Int
  , lsTy          :: Map Id Ty
  , lsLoopVars    :: Set Id
  , lsHeaderMap   :: Map Id Int
  , lsConstS      :: Map Word32 Int
  , lsConstV      :: Map Word32 Int
  , lsBcast       :: Map Int Int
  , lsOpsRev      :: [MachineOp]
  , lsAddrOff     :: Map Buffer Int
  , lsHdrBase     :: !Int
  , lsTmpAddrIdx  :: !Int
  , lsTmpAddrVal  :: !Int
  , lsTmpAddrV    :: !Int
  }

type LowerM a = State LowerSt a

initLowerSt :: Int -> Map Id Ty -> Set Id -> [(Id, Int)] -> LowerSt
initLowerSt w tys loopVars headerLets =
  let hdrBase = 0
      tmpAddrIdx = hdrBase + w
      tmpAddrVal = tmpAddrIdx + 1
      tmpAddrV = tmpAddrVal + 1
      startScratch = tmpAddrV + w
      headerMap = M.fromList headerLets
      idReg0 = M.fromList [ (i, hdrBase + slot) | (i, slot) <- headerLets ]
      (nextScratch, idReg1) =
        M.foldlWithKey' (allocId headerMap loopVars w) (startScratch, idReg0) tys
  in LowerSt
      { lsW = w
      , lsNextScratch = nextScratch
      , lsIdReg = idReg1
      , lsTy = tys
      , lsLoopVars = loopVars
      , lsHeaderMap = headerMap
      , lsConstS = M.empty
      , lsConstV = M.empty
      , lsBcast = M.empty
      , lsOpsRev = []
      , lsAddrOff = M.empty
      , lsHdrBase = hdrBase
      , lsTmpAddrIdx = tmpAddrIdx
      , lsTmpAddrVal = tmpAddrVal
      , lsTmpAddrV = tmpAddrV
      }

allocId :: Map Id Int -> Set Id -> Int -> (Int, Map Id Int) -> Id -> Ty -> (Int, Map Id Int)
allocId headerMap loopVars w (next, m) x ty
  | S.member x loopVars = (next, m)
  | M.member x headerMap = (next, m)
  | otherwise =
      let size = case ty of
                   I32     -> 1
                   Vec _ _ -> w
      in (next + size, M.insert x next m)

emit :: MachineSlot -> [Int] -> [Int] -> [MemAccess] -> LowerM ()
emit slot r wr mem =
  modify $ \st -> st { lsOpsRev = MachineOp slot r wr mem : lsOpsRev st }

allocScratch :: Int -> LowerM Int
allocScratch n = do
  st <- get
  let base = lsNextScratch st
  put st { lsNextScratch = base + n }
  pure base

sa :: Int -> ScratchAddr
sa = ScratchAddr

-- ---------------------------------------------------------------------------
-- Header prelude
-- ---------------------------------------------------------------------------

emitHeaderPrelude :: LowerM ()
emitHeaderPrelude = do
  st <- get
  addr0 <- getConstS 0
  let dst = lsHdrBase st
      w = lsW st
      rds = [addr0]
      writes = [dst .. dst + w - 1]
      slot = MSlotLoad (ISA.VLoad (sa dst) (sa addr0))
      mem = [MemAccess "header" "load" (Range 0 (w - 1))]
  emit slot rds writes mem

-- ---------------------------------------------------------------------------
-- Constants and broadcasts
-- ---------------------------------------------------------------------------

getConstS :: Word32 -> LowerM Int
getConstS c = do
  st <- get
  case M.lookup c (lsConstS st) of
    Just r  -> pure r
    Nothing -> do
      r <- allocScratch 1
      emit (MSlotLoad (ISA.Const (sa r) c)) [] [r] []
      modify $ \st' -> st' { lsConstS = M.insert c r (lsConstS st') }
      pure r

getConstV :: Word32 -> LowerM Int
getConstV c = do
  st <- get
  case M.lookup c (lsConstV st) of
    Just r  -> pure r
    Nothing -> do
      s <- getConstS c
      w <- gets lsW
      v <- allocScratch w
      emit (MSlotValu (ISA.VBroadcast (sa v) (sa s))) [s] [v .. v + w - 1] []
      modify $ \st' -> st' { lsConstV = M.insert c v (lsConstV st') }
      pure v

getBroadcast :: Int -> LowerM Int
getBroadcast sReg = do
  st <- get
  case M.lookup sReg (lsBcast st) of
    Just v  -> pure v
    Nothing -> do
      w <- gets lsW
      v <- allocScratch w
      emit (MSlotValu (ISA.VBroadcast (sa v) (sa sReg))) [sReg] [v .. v + w - 1] []
      modify $ \st' -> st' { lsBcast = M.insert sReg v (lsBcast st') }
      pure v

-- ---------------------------------------------------------------------------
-- Expr lowering
-- ---------------------------------------------------------------------------

data LoopEnv = LoopEnv
  { leReg :: Map Id Int
  , leVal :: Map Id Int
  }

emptyLoopEnv :: LoopEnv
emptyLoopEnv = LoopEnv M.empty M.empty

lookupIdReg :: LoopEnv -> Id -> LowerM Int
lookupIdReg env x = do
  st <- get
  case M.lookup x (leReg env) of
    Just r -> pure r
    Nothing ->
      case M.lookup x (lsIdReg st) of
        Just r  -> pure r
        Nothing -> error ("no scratch for id " ++ show x)

exprScalar :: LoopEnv -> Expr -> LowerM Int
exprScalar env e =
  case e of
    Const c -> getConstS c
    Var x   -> lookupIdReg env x

exprVector :: LoopEnv -> Expr -> LowerM Int
exprVector env e = do
  st <- get
  case e of
    Const c -> getConstV c
    Var x ->
      case M.lookup x (lsTy st) of
        Just (Vec _ _) -> lookupIdReg env x
        _              -> do
          s <- lookupIdReg env x
          getBroadcast s

exprOffset :: LoopEnv -> Expr -> Maybe Int
exprOffset env e =
  case e of
    Const c -> Just (fromIntegral c)
    Var x   -> M.lookup x (leVal env)

-- ---------------------------------------------------------------------------
-- Lowering statements (with unrolling)
-- ---------------------------------------------------------------------------

lowerTop :: [Stmt] -> LowerM ()
lowerTop ss = do
  emitHeaderPrelude
  lowerStmts emptyLoopEnv ss
  emit (MSlotFlow ISA.Halt) [] [] []

lowerStmts :: LoopEnv -> [Stmt] -> LowerM ()
lowerStmts env = mapM_ (lowerStmt env)

lowerStmt :: LoopEnv -> Stmt -> LowerM ()
lowerStmt env stmt =
  case stmt of
    Let x _ty rhs -> do
      st <- get
      if M.member x (lsHeaderMap st)
         then pure ()
         else lowerLet env x _ty rhs

    Store k buf ix v -> lowerStore env k buf ix v

    For iv s e stepN body ->
      let used = ivUsedInBody iv body
      in mapM_ (\i -> do
                  env' <- if used
                            then do
                              r <- getConstS (fromIntegral i)
                              pure env
                                { leReg = M.insert iv r (leReg env)
                                , leVal = M.insert iv i (leVal env)
                                }
                            else pure env
                  lowerStmts env' body
              ) [s, s + stepN .. e - 1]

lowerLet :: LoopEnv -> Id -> Ty -> Rhs -> LowerM ()
lowerLet env x ty rhs = do
  st <- get
  dst <- case M.lookup x (lsIdReg st) of
           Just r  -> pure r
           Nothing -> error ("no dst scratch for " ++ show x)
  case rhs of
    RConst c ->
      case ty of
        I32 ->
          emit (MSlotLoad (ISA.Const (sa dst) c)) [] [dst] []
        Vec _ _ -> do
          s <- getConstS c
          emit (MSlotValu (ISA.VBroadcast (sa dst) (sa s))) [s] (vecWrites st dst) []

    RBin op a b ->
      case ty of
        I32 -> do
          a1 <- exprScalar env a
          a2 <- exprScalar env b
          emit (MSlotAlu (ISA.Alu op (sa dst) (sa a1) (sa a2))) [a1,a2] [dst] []
        Vec _ _ -> do
          a1 <- exprVector env a
          a2 <- exprVector env b
          emit (MSlotValu (ISA.VAlu op (sa dst) (sa a1) (sa a2)))
               (vecReads st a1 ++ vecReads st a2) (vecWrites st dst) []

    RMulAdd a b c ->
      case ty of
        I32 -> do
          a1 <- exprScalar env a
          b1 <- exprScalar env b
          c1 <- exprScalar env c
          emit (MSlotAlu (ISA.Alu Mul (sa dst) (sa a1) (sa b1))) [a1,b1] [dst] []
          emit (MSlotAlu (ISA.Alu Add (sa dst) (sa dst) (sa c1))) [dst,c1] [dst] []
        Vec _ _ -> do
          a1 <- exprVector env a
          b1 <- exprVector env b
          c1 <- exprVector env c
          emit (MSlotValu (ISA.MultiplyAdd (sa dst) (sa a1) (sa b1) (sa c1)))
               (vecReads st a1 ++ vecReads st b1 ++ vecReads st c1) (vecWrites st dst) []

    RSelect c a b ->
      case ty of
        I32 -> do
          rc <- exprScalar env c
          ra <- exprScalar env a
          rb <- exprScalar env b
          emit (MSlotFlow (ISA.Select (sa dst) (sa rc) (sa ra) (sa rb))) [rc,ra,rb] [dst] []
        Vec _ _ -> do
          rc <- exprVector env c
          ra <- exprVector env a
          rb <- exprVector env b
          emit (MSlotFlow (ISA.VSelect (sa dst) (sa rc) (sa ra) (sa rb)))
               (vecReads st rc ++ vecReads st ra ++ vecReads st rb) (vecWrites st dst) []

    RVBroadcast _w e -> do
      s <- exprScalar env e
      emit (MSlotValu (ISA.VBroadcast (sa dst) (sa s))) [s] (vecWrites st dst) []

    RLoad k buf ix ->
      case k of
        LoadScalar      -> lowerLoadScalar env dst buf ix
        LoadContigVec _ -> lowerLoadVecContig env dst buf ix
        LoadGatherVec _ -> lowerLoadVecGather env dst buf ix

-- ---------------------------------------------------------------------------
-- Address computation
-- ---------------------------------------------------------------------------

computeAddr :: LoopEnv -> Buffer -> Expr -> LowerM Int
computeAddr env buf ix = do
  st <- get
  base <- basePtr st buf
  offR <- exprScalar env ix
  let addr = addrTmpFor st buf
      mOff = exprOffset env ix
      cached = M.lookup buf (lsAddrOff st)
  case (mOff, cached) of
    (Just o, Just o') | o == o' -> pure addr
    _ -> do
      emit (MSlotAlu (ISA.Alu Add (sa addr) (sa base) (sa offR))) [base,offR] [addr] []
      modify $ \st' ->
        st' { lsAddrOff = case mOff of
                            Just o  -> M.insert buf o (lsAddrOff st')
                            Nothing -> M.delete buf (lsAddrOff st')
            }
      pure addr

-- ---------------------------------------------------------------------------
-- Loads and stores
-- ---------------------------------------------------------------------------

lowerStore :: LoopEnv -> StoreKind -> Buffer -> Expr -> Expr -> LowerM ()
lowerStore env k buf ix v = do
  st <- get
  addr <- computeAddr env buf ix
  case k of
    StoreScalar -> do
      src <- exprScalar env v
      let mem = [MemAccess (regionKey buf) "store" (range1 env ix)]
      emit (MSlotStore (ISA.Store (sa addr) (sa src))) [addr,src] [] mem
    StoreContigVec _ -> do
      src <- exprVector env v
      let mem = [MemAccess (regionKey buf) "store" (rangeVec st env ix)]
      emit (MSlotStore (ISA.VStore (sa addr) (sa src))) ([addr] ++ vecReads st src) [] mem

lowerLoadScalar :: LoopEnv -> Int -> Buffer -> Expr -> LowerM ()
lowerLoadScalar env dst buf ix = do
  addr <- computeAddr env buf ix
  let mem = [MemAccess (regionKey buf) "load" (range1 env ix)]
  emit (MSlotLoad (ISA.Load (sa dst) (sa addr))) [addr] [dst] mem

lowerLoadVecContig :: LoopEnv -> Int -> Buffer -> Expr -> LowerM ()
lowerLoadVecContig env dst buf ix = do
  st <- get
  addr <- computeAddr env buf ix
  let mem = [MemAccess (regionKey buf) "load" (rangeVec st env ix)]
  emit (MSlotLoad (ISA.VLoad (sa dst) (sa addr))) [addr] (vecWrites st dst) mem

lowerLoadVecGather :: LoopEnv -> Int -> Buffer -> Expr -> LowerM ()
lowerLoadVecGather env dst buf ix = do
  st <- get
  baseS <- basePtr st buf
  baseV <- getBroadcast baseS
  ixV   <- exprVector env ix
  let addrV = lsTmpAddrV st
      w = lsW st
  emit (MSlotValu (ISA.VAlu Add (sa addrV) (sa ixV) (sa baseV)))
       (vecReads st ixV ++ vecReads st baseV) (vecWrites st addrV) []
  mapM_ (\lane -> do
            let dstLane = dst + lane
                addrLane = addrV + lane
                mem = [MemAccess (regionKey buf) "load" Unknown]
            emit (MSlotLoad (ISA.LoadOffset (sa dst) (sa addrV) (Offset lane)))
                 [addrLane] [dstLane] mem
        ) [0 .. w - 1]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

basePtr :: LowerSt -> Buffer -> LowerM Int
basePtr st buf =
  case buf of
    Header -> getConstS 0
    Forest -> pure (lsHdrBase st + 4)
    InpIdx -> pure (lsHdrBase st + 5)
    InpVal -> pure (lsHdrBase st + 6)

addrTmpFor :: LowerSt -> Buffer -> Int
addrTmpFor st buf =
  case buf of
    InpVal -> lsTmpAddrVal st
    _      -> lsTmpAddrIdx st

regionKey :: Buffer -> String
regionKey buf =
  case buf of
    Header -> "header"
    Forest -> "forest"
    InpIdx -> "idx"
    InpVal -> "val"

range1 :: LoopEnv -> Expr -> MemRange
range1 env ix =
  case exprOffset env ix of
    Just o  -> Range o o
    Nothing -> Unknown

rangeVec :: LowerSt -> LoopEnv -> Expr -> MemRange
rangeVec st env ix =
  case exprOffset env ix of
    Just o  -> Range o (o + lsW st - 1)
    Nothing -> Unknown

vecReads :: LowerSt -> Int -> [Int]
vecReads st base = [base .. base + lsW st - 1]

vecWrites :: LowerSt -> Int -> [Int]
vecWrites = vecReads

-- ---------------------------------------------------------------------------
-- IR traversals
-- ---------------------------------------------------------------------------

collectTypes :: Function -> Map Id Ty
collectTypes (Function ss) = goBlock M.empty ss
  where
    goBlock env = foldl' goStmt env
    goStmt env stmt =
      case stmt of
        Let x ty _ -> M.insert x ty env
        Store{}    -> env
        For iv _ _ _ body ->
          let env1 = M.insert iv I32 env
          in goBlock env1 body

collectLoopVars :: Function -> Set Id
collectLoopVars (Function ss) = goBlock S.empty ss
  where
    goBlock acc = foldl' go acc
    go acc stmt =
      case stmt of
        For iv _ _ _ body -> goBlock (S.insert iv acc) body
        _                 -> acc

takeHeaderLets :: [Stmt] -> [(Id, Int)]
takeHeaderLets = goH []
  where
    goH acc (Let x I32 (RLoad LoadScalar Header (Const c)) : rest) =
      goH (acc ++ [(x, fromIntegral c)]) rest
    goH acc _ = acc

ivUsedInBody :: Id -> [Stmt] -> Bool
ivUsedInBody iv = any usesStmt
  where
    usesStmt stmt =
      case stmt of
        Let _ _ rhs -> usesRhs rhs
        Store _ _ ix v -> usesExpr ix || usesExpr v
        For{} -> False
    usesRhs rhs =
      case rhs of
        RConst _ -> False
        RBin _ a b -> usesExpr a || usesExpr b
        RMulAdd a b c -> usesExpr a || usesExpr b || usesExpr c
        RSelect c a b -> usesExpr c || usesExpr a || usesExpr b
        RLoad _ _ ix -> usesExpr ix
        RVBroadcast _ e -> usesExpr e
    usesExpr e =
      case e of
        Var x   -> x == iv
        Const _ -> False
