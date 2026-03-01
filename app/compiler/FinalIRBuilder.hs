module FinalIRBuilder
  ( BuilderState(..)
  , BuildM
  , freshId
  , constId
  , bin
  , load
  , store
  , select
  , addr0
  , constStmt
  , memTy
  ) where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified FinalIR
import ISA (AluOp)

data BuilderState = BuilderState
  { bsNextId :: !FinalIR.Id
  , bsConstMap :: !(M.Map Int FinalIR.Id)
  , bsConstDefsRev :: ![(Int, FinalIR.Id)]
  }

type BuildM = State BuilderState

memTy :: FinalIR.Ty
memTy = FinalIR.Mem FinalIR.MemAny

constStmt :: (Int, FinalIR.Id) -> FinalIR.Stmt k
constStmt (val, outId) =
  FinalIR.Let
    { FinalIR.letOuts = [(outId, FinalIR.I32)]
    , FinalIR.letRhs = FinalIR.RConst (fromIntegral val)
    }

addr0 :: FinalIR.Id -> FinalIR.Addr
addr0 baseId =
  FinalIR.Addr
    { FinalIR.addrBase = baseId
    , FinalIR.addrIndex = FinalIR.IndexAff (FinalIR.IxConst 0)
    }

freshId :: BuildM FinalIR.Id
freshId = do
  st <- get
  let i = bsNextId st
  put st { bsNextId = i + 1 }
  pure i

constId :: Int -> BuildM FinalIR.Id
constId val = do
  st <- get
  case M.lookup val (bsConstMap st) of
    Just i -> pure i
    Nothing -> do
      i <- freshId
      modify' $ \s ->
        s
          { bsConstMap = M.insert val i (bsConstMap s)
          , bsConstDefsRev = (val, i) : bsConstDefsRev s
          }
      pure i

bin
  :: FinalIR.Ty
  -> AluOp
  -> FinalIR.Id
  -> FinalIR.Id
  -> BuildM (FinalIR.Id, FinalIR.Stmt k)
bin outTy op a b = do
  out <- freshId
  pure
    ( out
    , FinalIR.Let
        { FinalIR.letOuts = [(out, outTy)]
        , FinalIR.letRhs = FinalIR.RBin op a b
        }
    )

select
  :: FinalIR.Ty
  -> FinalIR.Id
  -> FinalIR.Id
  -> FinalIR.Id
  -> BuildM (FinalIR.Id, FinalIR.Stmt k)
select outTy cond trueVal falseVal = do
  out <- freshId
  pure
    ( out
    , FinalIR.Let
        { FinalIR.letOuts = [(out, outTy)]
        , FinalIR.letRhs = FinalIR.RSelect cond trueVal falseVal
        }
    )

load
  :: FinalIR.Ty
  -> FinalIR.Id
  -> FinalIR.Addr
  -> BuildM (FinalIR.Id, FinalIR.Id, FinalIR.Stmt k)
load outTy memIn addr = do
  outVal <- freshId
  memOut <- freshId
  pure
    ( outVal
    , memOut
    , FinalIR.Let
        { FinalIR.letOuts = [(outVal, outTy), (memOut, memTy)]
        , FinalIR.letRhs =
            FinalIR.RLoad
              { FinalIR.memIn = memIn
              , FinalIR.loadAddr = addr
              }
        }
    )

store
  :: FinalIR.Id
  -> FinalIR.Addr
  -> FinalIR.Id
  -> BuildM (FinalIR.Id, FinalIR.Stmt k)
store memIn addr val = do
  memOut <- freshId
  pure
    ( memOut
    , FinalIR.Let
        { FinalIR.letOuts = [(memOut, memTy)]
        , FinalIR.letRhs =
            FinalIR.RStore
              { FinalIR.memIn = memIn
              , FinalIR.storeAddr = addr
              , FinalIR.storeVal = val
              }
        }
    )
