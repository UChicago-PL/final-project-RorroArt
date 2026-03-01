module FinalIRUtils
  ( -- * Type collection
    collectKernelTypes
  , collectRegionTypes
  , collectStmtTypes
  , insertTy
  , isMemTy
  , widthToInt

    -- * IR identifier extraction
  , rhsUses
  , effUses
  , addrUses
  , indexUses
  , ixUses
  , collectUses
  , stmtUses
  , regionUses
  , allIdsKernel
  , allIdsRegion
  , allIdsStmt
  , maxIdKernel
  ) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import FinalIR
  ( Addr(..)
  , Effect(..)
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

--------------------------------------------------------------------------------
-- Type collection
--------------------------------------------------------------------------------

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

isMemTy :: Ty -> Bool
isMemTy ty = case ty of
  Mem _ -> True
  _ -> False

widthToInt :: Width -> Int
widthToInt (Width w) = w

--------------------------------------------------------------------------------
-- IR identifier extraction
--
-- Leaf-node extractors (rhsUses, effUses, ...) extract referenced IDs from
-- individual nodes. The two higher-level families differ in scope:
--   collectUses / stmtUses / regionUses  -- referenced IDs only (for DCE)
--   allIdsKernel / allIdsRegion / allIdsStmt -- all IDs (for fresh-name gen)
--------------------------------------------------------------------------------

rhsUses :: Rhs -> [Id]
rhsUses rhs = case rhs of
  RConst _           -> []
  RBin _ a b         -> [a, b]
  RReduce _ a        -> [a]
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

-- | All referenced IDs in a statement list (no definition sites).
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
regionUses region =
  foldMap stmtUses (regionStmts region) <> S.fromList (regionYield region)

-- | All IDs (definitions + uses) for fresh-name generation.
maxIdKernel :: Kernel k -> Int
maxIdKernel kernel =
  case S.lookupMax (allIdsKernel kernel) of
    Nothing     -> -1
    Just (Id n) -> n

allIdsKernel :: Kernel k -> S.Set Id
allIdsKernel kernel =
  S.fromList (map fst (kernelParams kernel)) <> allIdsRegion (kernelBody kernel)

allIdsRegion :: Region k -> S.Set Id
allIdsRegion region =
  S.fromList (map fst (regionParams region))
    <> foldMap allIdsStmt (regionStmts region)
    <> S.fromList (regionYield region)

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
