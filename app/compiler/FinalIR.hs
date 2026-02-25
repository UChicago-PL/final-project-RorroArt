{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | structured optimization IR + scheduled machine IR.
module FinalIR
  ( -- * KernelIR
    Width(..)
  , Exec(..)
  , MemSpace(..)
  , Ty(..)
  , Id(..)
  , Ix(..)
  , Index(..)
  , Addr(..)
  , Rhs(..)
  , Effect(..)
  , Stmt(..)
  , Region(..)
  , Kernel(..)

    -- * BundleIR
  , Label(..)
  , BProgram(..)
  , BBlock(..)
  , BBundle(..)
  , BAluSlot(..)
  , BValuSlot(..)
  , BLoadSlot(..)
  , BStoreSlot(..)
  , BFlowSlot(..)
  , BDebugSlot(..)

    -- * Machine model
  , Engine(..)
  , MachineModel(..)

    -- * types
  , AluOp(..)
  ) where

import Data.Word (Word32)
import ISA (AluOp(..), Imm(..), Offset(..))

--------------------------------------------------------------------------------
-- KernelIR
--------------------------------------------------------------------------------

-- | SIMD width.
newtype Width = Width Int
  deriving (Show, Eq, Ord, Num)

-- | Execution mode for loops.
data Exec
  = ExecScalar
  | ExecSimd !Width
  deriving (Show, Eq, Ord)

-- | Optional memory space partitioning for effect ordering.
data MemSpace
  = MemAny
  | MemSpace !Int
  deriving (Show, Eq, Ord)

-- | KernelIR types.
data Ty
  = I32
  | Ptr
  | Vec !Width !Ty
  | Mask !Width
  | Mem !MemSpace
  deriving (Show, Eq, Ord)

-- | SSA value ID.
newtype Id = Id Int
  deriving (Show, Eq, Ord, Enum, Num)

-- | Affine index expressions.
data Ix
  = IxConst !Int
  | IxVar !Id
  | IxLane
  | IxAdd !Ix !Ix
  | IxMul !Int !Ix
  deriving (Show, Eq, Ord)

-- | Index can be affine (contiguous/vectorization-friendly) or arbitrary.
data Index
  = IndexAff !Ix
  | IndexVal !Id
  deriving (Show, Eq, Ord)

-- | Address = base pointer + index.
data Addr = Addr
  { addrBase :: !Id
  , addrIndex :: !Index
  }
  deriving (Show, Eq, Ord)

-- | Value-producing RHS operations.
data Rhs
  = RConst !Word32
  | RBin !AluOp !Id !Id
  | RSelect !Id !Id !Id
  | RCoreId
  | RBroadcast !Width !Id
  | RMultiplyAdd !Id !Id !Id
  | RLoad {memIn :: !Id, loadAddr :: !Addr}
  | RStore {memIn :: !Id, storeAddr :: !Addr, storeVal :: !Id}
  deriving (Show, Eq)

-- | Side effects that do not naturally fit as value-producing ops.
data Effect k
  = EPause
  | ETraceWrite !Id
  | EDebugCompare !Id !k
  | EDebugCompareV !Id ![k]
  | EDebugComment !String
  | EHalt
  deriving (Show, Eq, Ord, Functor)

-- | Region-SSA block.
data Region k = Region
  { regionParams :: ![(Id, Ty)]
  , regionStmts :: ![Stmt k]
  , regionYield :: ![Id]
  }
  deriving (Show, Eq, Functor)

-- | Structured statements.
data Stmt k
  = Let
      { letOuts :: ![(Id, Ty)]
      , letRhs :: !Rhs
      }
  | Eff !(Effect k)
  | If
      { ifCond :: !Id
      , ifThen :: !(Region k)
      , ifElse :: !(Region k)
      , ifOuts :: ![(Id, Ty)]
      }
  | For
      { forExec :: !Exec
      , forLb :: !Int
      , forUb :: !Id
      , forStep :: !Int
      , forInits :: ![Id]
      , forBody :: !(Region k)
      , forOuts :: ![(Id, Ty)]
      }
  deriving (Show, Eq, Functor)

-- | Top-level kernel.
data Kernel k = Kernel
  { kernelParams :: ![(Id, Ty)]
  , kernelRetTys :: ![Ty]
  , kernelBody :: !(Region k)
  }
  deriving (Show, Eq, Functor)

--------------------------------------------------------------------------------
-- BundleIR (scheduled IR)
--------------------------------------------------------------------------------

-- | Labels for CFG / jumps.
newtype Label = Label Int
  deriving (Show, Eq, Ord, Enum, Num)

-- | Scheduled program of cycle bundles.
data BProgram k r = BProgram
  { bEntry :: !Label
  , bBlocks :: ![BBlock k r]
  }
  deriving (Show, Eq)

data BBlock k r = BBlock
  { bLabel :: !Label
  , bBundles :: ![BBundle k r]
  }
  deriving (Show, Eq)

-- | One cycle worth of work across engines.
data BBundle k r = BBundle
  { bAluSlots :: ![BAluSlot r]
  , bValuSlots :: ![BValuSlot r]
  , bLoadSlots :: ![BLoadSlot r]
  , bStoreSlots :: ![BStoreSlot r]
  , bFlowSlots :: ![BFlowSlot r]
  , bDebugSlots :: ![BDebugSlot r k]
  }
  deriving (Show, Eq)

data BAluSlot r
  = BAlu !AluOp !r !r !r
  deriving (Show, Eq)

data BValuSlot r
  = BVBroadcast !r !r
  | BMultiplyAdd !r !r !r !r
  | BVAlu !AluOp !r !r !r
  deriving (Show, Eq)

data BLoadSlot r
  = BLoad !r !r
  | BLoadOffset !r !r !Offset
  | BVLoad !r !r
  | BConst !r !Word32
  deriving (Show, Eq)

data BStoreSlot r
  = BStore !r !r
  | BVStore !r !r
  deriving (Show, Eq)

-- | Control-flow slots reference labels; emission resolves labels to addresses.
data BFlowSlot r
  = BSelect !r !r !r !r
  | BAddImm !r !r !Imm
  | BVSelect !r !r !r !r
  | BHalt
  | BPause
  | BTraceWrite !r
  | BCondJump !r !Label
  | BJump !Label
  | BJumpIndirect !r
  | BCoreId !r
  deriving (Show, Eq)

data BDebugSlot r k
  = BCompare !r !k
  | BVCompare !r ![k]
  | BDebugIgnored !String
  deriving (Show, Eq, Ord, Functor)

--------------------------------------------------------------------------------
-- Machine model
--------------------------------------------------------------------------------

data Engine = EAlu | EValu | ELoad | EStore | EFlow | EDebug
  deriving (Show, Eq, Ord)

data MachineModel = MachineModel
  { mmSimdWidth :: !Width
  , mmCapAlu :: !Int
  , mmCapValu :: !Int
  , mmCapLoad :: !Int
  , mmCapStore :: !Int
  , mmCapFlow :: !Int
  , mmCapDebug :: !Int
  , mmLatAlu :: !Int
  , mmLatValu :: !Int
  , mmLatLoad :: !Int
  , mmLatStore :: !Int
  , mmLatFlow :: !Int
  , mmLatDebug :: !Int
  }
  deriving (Show, Eq)
