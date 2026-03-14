module Machine
  ( MachineSlot(..)
  , MemRange(..)
  , MemAccess(..)
  , MachineOp(..)
  ) where

import ISA (AluSlot, ValuSlot, LoadSlot, StoreSlot, FlowSlot)

-- | Typed machine slot wrapping ISA slot ADTs.
data MachineSlot
  = MSlotAlu   !AluSlot
  | MSlotValu  !ValuSlot
  | MSlotLoad  !LoadSlot
  | MSlotStore !StoreSlot
  | MSlotFlow  !FlowSlot
  deriving (Show, Eq)

-- | Memory range (word offsets) for bundling legality.
data MemRange
  = Unknown
  | Range !Int !Int
  deriving (Show, Eq, Ord)

-- | Memory access metadata.
data MemAccess = MemAccess
  { maRegion :: String      -- "header" | "forest" | "idx" | "val"
  , maKind   :: String      -- "load" | "store"
  , maRange  :: MemRange
  }
  deriving (Show, Eq, Ord)

-- | A single machine op (one simulator slot) plus bundling metadata.
data MachineOp = MachineOp
  { moSlot   :: !MachineSlot
  , moReads  :: ![Int]
  , moWrites :: ![Int]
  , moMem    :: ![MemAccess]
  }
  deriving (Show, Eq)
