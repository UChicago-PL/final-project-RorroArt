module Sim.Types
  ( SimConfig (..),
    CoreRunState (..),
    CoreState (..),
    MachineState (..),
    StepSnapshot (..),
    PendingWrites (..),
    SimError (..),
    SimM,
    StepM,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import Data.IntMap.Strict (IntMap)
import Data.Word (Word32)
import ISA qualified

-- Runtime options that do not change during execution.
data SimConfig = SimConfig
  { scMachineConfig :: !ISA.MachineConfig,
    scScratchSize :: !Int,
    scEnablePause :: !Bool
  }
  deriving (Show, Eq)

-- Per-core run state.
data CoreRunState
  = CoreRunning
  | CorePaused
  | CoreStopped
  deriving (Show, Eq)

-- Single-core state (Matching anthropic's assigment)
data CoreState = CoreState
  { csId :: !Int,
    csPc :: !ISA.ProgAddr,
    csRunState :: !CoreRunState,
    csScratch :: !(IntMap Word32),
    csTraceBuf :: ![Word32]
  }
  deriving (Show, Eq)

-- Whole-machine mutable state.
data MachineState = MachineState
  { msCore :: !CoreState,
    msMem :: !(IntMap Word32),
    msCycle :: !Int
  }
  deriving (Show, Eq)

-- Snapshot used during one bundle so reads are stable for the whole cycle.
data StepSnapshot = StepSnapshot
  { ssPc :: !ISA.ProgAddr,
    ssScratch :: !(IntMap Word32),
    ssMem :: !(IntMap Word32),
    ssEnablePause :: !Bool
  }
  deriving (Show, Eq)

-- Writes buffered during a bundle and committed at end of cycle.
data PendingWrites = PendingWrites
  { pwScratch :: !(IntMap Word32),
    pwMem :: !(IntMap Word32),
    pwPc :: !(Maybe ISA.ProgAddr),
    pwRunState :: !(Maybe CoreRunState)
  }
  deriving (Show, Eq)

data SimError
  = SimParseError !String
  | SimInvalidProgram !String
  | SimScratchOob !Int
  | SimMemoryOob !Int
  | SimUnknownPc !Int
  | SimDivideByZero !ISA.AluOp !Word32 !Word32
  | SimUnsupported !String
  deriving (Show, Eq)

type SimM a =
  ReaderT SimConfig (StateT MachineState (ExceptT SimError IO)) a

type StepM a =
  ReaderT StepSnapshot (StateT PendingWrites (Either SimError)) a
