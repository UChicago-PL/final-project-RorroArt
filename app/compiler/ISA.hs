{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ISA
  ( ScratchAddr(..)
  , ProgAddr(..)
  , Imm(..)
  , Offset(..)
  , Bundle(..)
  , AluOp(..)
  , AluSlot(..)
  , ValuSlot(..)
  , LoadSlot(..)
  , StoreSlot(..)
  , FlowSlot(..)
  , DebugSlot(..)
  ) where

import Data.Word (Word32)

newtype ScratchAddr = ScratchAddr Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype ProgAddr = ProgAddr Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype Imm = Imm Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype Offset = Offset Int
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

-- One VLIW bundle (one cycle worth of work across engines)
data Bundle k = Bundle
  { aluSlots :: [AluSlot]
  , valuSlots :: [ValuSlot]
  , loadSlots :: [LoadSlot]
  , storeSlots :: [StoreSlot]
  , flowSlots :: [FlowSlot]
  , debugSlots :: [DebugSlot k]
  } deriving (Show, Eq, Functor)

data AluOp
  = Add | Sub | Mul | Div | Cdiv
  | Xor | And | Or | Shl | Shr | Mod
  | Lt | Eq_
  deriving (Show, Eq)

-- ("op", dest, a1, a2)
data AluSlot = Alu AluOp ScratchAddr ScratchAddr ScratchAddr
  deriving (Show, Eq)

data ValuSlot
  = VBroadcast ScratchAddr ScratchAddr
  | MultiplyAdd ScratchAddr ScratchAddr ScratchAddr ScratchAddr
  | VAlu AluOp ScratchAddr ScratchAddr ScratchAddr
  deriving (Show, Eq)

data LoadSlot
  = Load ScratchAddr ScratchAddr
  | LoadOffset ScratchAddr ScratchAddr Offset
  | VLoad ScratchAddr ScratchAddr
  | Const ScratchAddr Word32
  deriving (Show, Eq)

data StoreSlot
  = Store ScratchAddr ScratchAddr
  | VStore ScratchAddr ScratchAddr
  deriving (Show, Eq)

data FlowSlot
  = Select ScratchAddr ScratchAddr ScratchAddr ScratchAddr
  | AddImm ScratchAddr ScratchAddr Imm
  | VSelect ScratchAddr ScratchAddr ScratchAddr ScratchAddr
  | Halt
  | Pause
  | TraceWrite ScratchAddr
  | CondJump ScratchAddr ProgAddr
  | CondJumpRel ScratchAddr Offset
  | Jump ProgAddr
  | JumpIndirect ScratchAddr
  | CoreId ScratchAddr
  deriving (Show, Eq)

data DebugSlot k
  = Compare ScratchAddr k
  | VCompare ScratchAddr [k]
  | DebugIgnored String      -- machine ignores other debug ops (e.g. "comment")
  deriving (Show, Eq, Functor)
