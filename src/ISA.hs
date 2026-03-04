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
  , Engine(..)
  , SlotLimits(..)
  , MachineConfig(..)
  , defaultSlotLimits
  , defaultMachineConfig
  , opcodeArity
  , parseAluOp
  , renderAluOp
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
  deriving (Show, Eq, Ord)

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
  | DebugIgnored String
  deriving (Show, Eq, Functor)

data Engine = EngineAlu | EngineValu | EngineLoad | EngineStore | EngineFlow
  deriving (Show, Eq, Ord)

data SlotLimits = SlotLimits
  { slAlu :: !Int
  , slValu :: !Int
  , slLoad :: !Int
  , slStore :: !Int
  , slFlow :: !Int
  , slDebug :: !Int
  } deriving (Show, Eq)

newtype MachineConfig = MachineConfig
  { mcSlotLimits :: SlotLimits
  } deriving (Show, Eq)

defaultSlotLimits :: SlotLimits
defaultSlotLimits =
  SlotLimits
    { slAlu = 12
    , slValu = 6
    , slLoad = 2
    , slStore = 2
    , slFlow = 1
    , slDebug = 64
    }

defaultMachineConfig :: MachineConfig
defaultMachineConfig =
  MachineConfig
    { mcSlotLimits = defaultSlotLimits
    }

-- | Expected argument counts per opcode, excluding the opcode string itself.
opcodeArity :: Engine -> String -> Maybe Int
opcodeArity engine op = case engine of
  EngineAlu ->
    case parseAluOp op of
      Just _ -> Just 3
      Nothing -> Nothing
  EngineValu ->
    case op of
      "vbroadcast" -> Just 2
      "multiply_add" -> Just 4
      _ ->
        case parseAluOp op of
          Just _ -> Just 3
          Nothing -> Nothing
  EngineLoad ->
    case op of
      "load" -> Just 2
      "load_offset" -> Just 3
      "vload" -> Just 2
      "const" -> Just 2
      _ -> Nothing
  EngineStore ->
    case op of
      "store" -> Just 2
      "vstore" -> Just 2
      _ -> Nothing
  EngineFlow ->
    case op of
      "select" -> Just 4
      "add_imm" -> Just 3
      "vselect" -> Just 4
      "halt" -> Just 0
      "pause" -> Just 0
      "trace_write" -> Just 1
      "cond_jump" -> Just 2
      "cond_jump_rel" -> Just 2
      "jump" -> Just 1
      "jump_indirect" -> Just 1
      "coreid" -> Just 1
      _ -> Nothing

parseAluOp :: String -> Maybe AluOp
parseAluOp op = case op of
  "+" -> Just Add
  "-" -> Just Sub
  "*" -> Just Mul
  "//" -> Just Div
  "cdiv" -> Just Cdiv
  "^" -> Just Xor
  "&" -> Just And
  "|" -> Just Or
  "<<" -> Just Shl
  ">>" -> Just Shr
  "%" -> Just Mod
  "<" -> Just Lt
  "==" -> Just Eq_
  _ -> Nothing

renderAluOp :: AluOp -> String
renderAluOp op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "//"
  Cdiv -> "cdiv"
  Xor -> "^"
  And -> "&"
  Or -> "|"
  Shl -> "<<"
  Shr -> ">>"
  Mod -> "%"
  Lt -> "<"
  Eq_ -> "=="
