module Sim.StateAccess
  ( readScratch,
    readMemAt,
    readMemRaw,
    writeScratch,
    writeMemAt,
    writeMemRaw,
    setPc,
    setCoreRunState,
    ensureScratchInBounds,
    ensureMemInBounds,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (MonadTrans (lift), ask)
import Control.Monad.State.Strict (modify')
import Data.IntMap qualified as IM
import Data.Word (Word32)
import ISA qualified
import Sim.MemoryIO (word32ToInt)
import Sim.Types

-- Snapshot reads + pending writes

scratchAddrToInt :: ISA.ScratchAddr -> Int
scratchAddrToInt (ISA.ScratchAddr i) = i

readScratch :: ISA.ScratchAddr -> StepM Word32
readScratch sa = do
  let addr = scratchAddrToInt sa
  ensureScratchInBounds addr
  StepSnapshot {ssScratch} <- ask
  case IM.lookup addr ssScratch of
    Just w -> pure w
    Nothing -> pure 0 -- Assumption: Scratch is initialized to 0s

readMemAt :: ISA.ScratchAddr -> StepM Word32
readMemAt areg = do
  addrW <- readScratch areg
  let addr = word32ToInt addrW
  ensureMemInBounds addr
  StepSnapshot {ssMem} <- ask
  case IM.lookup addr ssMem of
    Just w -> pure w
    Nothing -> pure 0 -- Assumption: Memory is initialized to 0s

readMemRaw :: Int -> StepM Word32
readMemRaw addr = do
  ensureMemInBounds addr
  StepSnapshot {ssMem} <- ask
  case IM.lookup addr ssMem of
    Just w -> pure w
    Nothing -> pure 0

writeScratch :: ISA.ScratchAddr -> Word32 -> StepM ()
writeScratch sa val = do
  let addr = scratchAddrToInt sa
  ensureScratchInBounds addr
  modify' $ \pw -> pw {pwScratch = IM.insert addr val (pwScratch pw)}

writeMemAt :: ISA.ScratchAddr -> ISA.ScratchAddr -> StepM ()
writeMemAt areg memaddr = do
  addrW <- readScratch areg
  let addr = word32ToInt addrW
  ensureMemInBounds addr
  memval <- readScratch memaddr
  modify' $ \pw -> pw {pwMem = IM.insert addr memval (pwMem pw)}

writeMemRaw :: Int -> Word32 -> StepM ()
writeMemRaw addr val = do
  ensureMemInBounds addr
  modify' $ \pw -> pw {pwMem = IM.insert addr val (pwMem pw)}

setPc :: ISA.ProgAddr -> StepM ()
setPc pc = modify' $ \pw -> pw {pwPc = Just pc}

setCoreRunState :: CoreRunState -> StepM ()
setCoreRunState rs = modify' $ \pw -> pw {pwRunState = Just rs}

-- Validation helpers

throwStep :: SimError -> StepM a
throwStep e = lift (lift (Left e))

ensureScratchInBounds :: Int -> StepM ()
ensureScratchInBounds i = do
  StepSnapshot {ssScratch} <- ask
  let n = IM.size ssScratch
  when (i < 0 || i >= n) (throwStep (SimScratchOob i))

ensureMemInBounds :: Int -> StepM ()
ensureMemInBounds i = do
  StepSnapshot {ssMem} <- ask
  let n = IM.size ssMem
  when (i < 0 || i >= n) (throwStep (SimMemoryOob i))
