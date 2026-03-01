module FinalIRUtils
  ( collectKernelTypes
  , collectRegionTypes
  , collectStmtTypes
  , insertTy
  , isMemTy
  ) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import FinalIR (Id, Kernel(..), Region(..), Stmt(..), Ty(..))

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
