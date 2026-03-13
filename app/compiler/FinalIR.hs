module FinalIR
  ( Id(..)
  , Ty(..)
  , vWidth
  , Buffer(..)
  , LoadKind(..)
  , StoreKind(..)
  , Expr(..)
  , Rhs(..)
  , Stmt(..)
  , Function(..)
  , Build
  , runBuild
  , fresh
  , emitLet
  , emitStore
  , emitFor
  ) where

import Control.Monad.State.Strict (State, get, put, modify, runState)
import Data.Word (Word32)

import ISA (AluOp)

-- | SSA identifier.
newtype Id = Id { unId :: Int }
  deriving (Eq, Ord, Show)

-- | Types. Vec w I32 corresponds to a scratch block of size w.
data Ty
  = I32
  | Vec !Int !Ty
  deriving (Eq, Ord, Show)

vWidth :: Ty -> Maybe Int
vWidth (Vec w _) = Just w
vWidth _         = Nothing

-- | Named disjoint memory regions.
data Buffer
  = Header
  | Forest
  | InpIdx
  | InpVal
  deriving (Eq, Ord, Show)

-- | Memory load modes.
data LoadKind
  = LoadScalar
  | LoadContigVec !Int
  | LoadGatherVec !Int
  deriving (Eq, Ord, Show)

-- | Memory store modes.
data StoreKind
  = StoreScalar
  | StoreContigVec !Int
  deriving (Eq, Ord, Show)

data Expr
  = Var !Id
  | Const !Word32
  deriving (Eq, Ord, Show)

-- | RHS for let-bindings.  Uses AluOp from ISA.
data Rhs
  = RConst !Word32
  | RBin !AluOp !Expr !Expr
  | RMulAdd !Expr !Expr !Expr
  | RSelect !Expr !Expr !Expr
  | RLoad !LoadKind !Buffer !Expr
  | RVBroadcast !Int !Expr
  deriving (Eq, Ord, Show)

-- | Statements.
data Stmt
  = Let !Id !Ty !Rhs
  | Store !StoreKind !Buffer !Expr !Expr
  | For !Id !Int !Int !Int ![Stmt]   -- iv, start, end, step, body
  deriving (Eq, Ord, Show)

newtype Function = Function { fnBody :: [Stmt] }
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- IR builder
-- ---------------------------------------------------------------------------

data BuildSt = BuildSt
  { bsNext :: !Int
  , bsRev  :: ![Stmt]
  }

newtype Build a = Build { unBuild :: State BuildSt a }

instance Functor Build where
  fmap f (Build m) = Build (fmap f m)

instance Applicative Build where
  pure x = Build (pure x)
  Build f <*> Build x = Build (f <*> x)

instance Monad Build where
  Build m >>= k = Build (m >>= unBuild . k)

runBuild :: Build a -> (a, [Stmt])
runBuild (Build m) =
  let st0 = BuildSt { bsNext = 0, bsRev = [] }
      (a, st1) = runState m st0
  in (a, reverse (bsRev st1))

fresh :: Build Id
fresh = Build $ do
  st <- get
  let i = bsNext st
  put st { bsNext = i + 1 }
  pure (Id i)

emitStmt :: Stmt -> Build ()
emitStmt s = Build $ modify (\st -> st { bsRev = s : bsRev st })

emitLet :: Ty -> Rhs -> Build Expr
emitLet ty rhs = do
  x <- fresh
  emitStmt (Let x ty rhs)
  pure (Var x)

emitStore :: StoreKind -> Buffer -> Expr -> Expr -> Build ()
emitStore k buf ix v = emitStmt (Store k buf ix v)

emitFor :: Int -> Int -> Int -> (Expr -> Build ()) -> Build ()
emitFor start end step bodyFn = do
  iv <- fresh
  bodyStmts <- capture (bodyFn (Var iv))
  emitStmt (For iv start end step bodyStmts)

capture :: Build a -> Build [Stmt]
capture (Build m) = Build $ do
  st0 <- get
  put st0 { bsRev = [] }
  _ <- m
  st1 <- get
  put st0 { bsNext = bsNext st1, bsRev = bsRev st0 }
  pure (reverse (bsRev st1))
