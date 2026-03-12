module Sim.Exec
  ( buildSnapshot,
    emptyPendingWrites,
    executeBundle,
    executeBundleSlots,
    commitPendingWrites,
    resolveNextPc,
    executeAluSlot,
    executeValuSlot,
    executeLoadSlot,
    executeStoreSlot,
    executeFlowSlot,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (MonadTrans (lift), ask, runReaderT)
import Control.Monad.State.Strict (get, put, runStateT)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.IntMap qualified as IM
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import ISA qualified
import Sim.MemoryIO (intToWord32, word32ToInt)
import Sim.StateAccess
  ( readMemAt,
    readMemRaw,
    readScratch,
    setCoreRunState,
    setPc,
    writeMemRaw,
    writeScratch,
  )
import Sim.Types

buildSnapshot :: SimConfig -> MachineState -> StepSnapshot
buildSnapshot cfg ms =
  StepSnapshot
    { ssPc = csPc (msCore ms),
      ssScratch = csScratch (msCore ms),
      ssMem = msMem ms,
      ssEnablePause = scEnablePause cfg
    }

emptyPendingWrites :: PendingWrites
emptyPendingWrites =
  PendingWrites
    { pwScratch = IM.empty,
      pwMem = IM.empty,
      pwPc = Nothing,
      pwRunState = Nothing
    }

executeBundle :: ISA.Bundle () -> SimM PendingWrites
executeBundle bundle = do
  cfg <- ask
  ms <- get
  let snap = buildSnapshot cfg ms
  case runStateT (runReaderT (executeBundleSlots bundle) snap) emptyPendingWrites of
    Left err -> throwError err
    Right (_, pw) -> do
      commitPendingWrites pw
      pure pw

executeBundleSlots :: ISA.Bundle () -> StepM ()
executeBundleSlots bundle = do
  mapM_ executeAluSlot (ISA.aluSlots bundle)
  mapM_ executeValuSlot (ISA.valuSlots bundle)
  mapM_ executeLoadSlot (ISA.loadSlots bundle)
  mapM_ executeStoreSlot (ISA.storeSlots bundle)
  mapM_ executeFlowSlot (ISA.flowSlots bundle)

commitPendingWrites :: PendingWrites -> SimM ()
commitPendingWrites pw = do
  ms <- get
  let core0 = msCore ms
      scratch' = IM.union (pwScratch pw) (csScratch core0)
      mem' = IM.union (pwMem pw) (msMem ms)
      pc' = fromMaybe (csPc core0 + 1) (pwPc pw)
      rs' = fromMaybe (csRunState core0) (pwRunState pw)
      core' =
        core0
          { csScratch = scratch',
            csPc = pc',
            csRunState = rs'
          }
  put ms {msCore = core', msMem = mem'}

resolveNextPc :: StepSnapshot -> PendingWrites -> Either SimError ISA.ProgAddr
resolveNextPc snap pw = do
  let pc = fromMaybe (ssPc snap + 1) (pwPc pw)
      i = progAddrToInt pc
  if i < 0
    then Left (SimUnknownPc i)
    else Right pc

-- Engine dispatch

liftEitherStep :: Either SimError a -> StepM a
liftEitherStep = either throwStep pure

forLane8 :: (Int -> StepM ()) -> StepM ()
forLane8 = forM_ [0 .. 7]

executeAluSlot :: ISA.AluSlot -> StepM ()
executeAluSlot (ISA.Alu op dst srcA srcB) = do
  a <- readScratch srcA
  b <- readScratch srcB
  r <- liftEitherStep (aluEval op a b)
  writeScratch dst r

executeValuSlot :: ISA.ValuSlot -> StepM ()
executeValuSlot slot = case slot of
  ISA.VBroadcast dest src -> forLane8 (execVBroadcastLane dest src)
  ISA.MultiplyAdd dest a b c -> forLane8 (execMultiplyAddLane dest a b c)
  ISA.VAlu op dest a b -> forLane8 (execVAluLane op dest a b)

executeLoadSlot :: ISA.LoadSlot -> StepM ()
executeLoadSlot slot = case slot of
  ISA.Load dest addrReg -> execLoad dest addrReg
  ISA.LoadOffset dest addrReg (ISA.Offset off) -> execLoad (scratchPlus dest off) (scratchPlus addrReg off)
  ISA.VLoad dest addrReg -> execVLoad dest addrReg
  ISA.Const dest val -> writeScratch dest val

executeStoreSlot :: ISA.StoreSlot -> StepM ()
executeStoreSlot slot = case slot of
  ISA.Store addrReg srcReg -> execStore addrReg srcReg
  ISA.VStore addrReg srcReg -> execVStore addrReg srcReg

executeFlowSlot :: ISA.FlowSlot -> StepM ()
executeFlowSlot slot = case slot of
  ISA.Select dest cond a b -> execSelect dest cond a b
  ISA.AddImm dest a imm -> execAddImm dest a imm
  ISA.VSelect dest cond a b -> forLane8 (execVSelectLane dest cond a b)
  ISA.Halt -> execHalt
  ISA.Pause -> execPause
  ISA.TraceWrite src -> execTraceWrite src
  ISA.CondJump cond addr -> execCondJump cond addr
  ISA.CondJumpRel cond off -> execCondJumpRel cond off
  ISA.Jump addr -> setPc addr
  ISA.JumpIndirect addrReg -> execJumpIndirect addrReg
  ISA.CoreId dest -> execCoreId dest

-- Evaluators and execution helpers

scratchPlus :: ISA.ScratchAddr -> Int -> ISA.ScratchAddr
scratchPlus (ISA.ScratchAddr base) off = ISA.ScratchAddr (base + off)

progAddrToInt :: ISA.ProgAddr -> Int
progAddrToInt (ISA.ProgAddr n) = n

intToProgAddr :: Int -> ISA.ProgAddr
intToProgAddr = ISA.ProgAddr

isNonZero :: Word32 -> Bool
isNonZero w = w /= 0

aluEval :: ISA.AluOp -> Word32 -> Word32 -> Either SimError Word32
aluEval op a b = case op of
  ISA.Add -> Right (a + b)
  ISA.Sub -> Right (a - b)
  ISA.Mul -> Right (a * b)
  ISA.Div -> guardDiv op $ a `div` b
  ISA.Cdiv -> guardDiv op $ let (q, r) = a `divMod` b in if r == 0 then q else q + 1
  ISA.Mod -> guardDiv op $ a `mod` b
  ISA.And -> Right (a .&. b)
  ISA.Or -> Right (a .|. b)
  ISA.Xor -> Right (a `xor` b)
  ISA.Shl -> Right (a `shiftL` fromIntegral b)
  ISA.Shr -> Right (a `shiftR` fromIntegral b)
  ISA.Lt -> Right (if a < b then 1 else 0)
  ISA.Eq_ -> Right (if a == b then 1 else 0)
  where
    guardDiv o v
      | b == 0 = Left (SimDivideByZero o a b)
      | otherwise = Right v

execVBroadcastLane :: ISA.ScratchAddr -> ISA.ScratchAddr -> Int -> StepM ()
execVBroadcastLane dest src lane = do
  x <- readScratch src
  writeScratch (scratchPlus dest lane) x

execMultiplyAddLane :: ISA.ScratchAddr -> ISA.ScratchAddr -> ISA.ScratchAddr -> ISA.ScratchAddr -> Int -> StepM ()
execMultiplyAddLane dest a b c lane = do
  x <- readScratch (scratchPlus a lane)
  y <- readScratch (scratchPlus b lane)
  z <- readScratch (scratchPlus c lane)
  writeScratch (scratchPlus dest lane) (x * y + z)

execVAluLane ::
  ISA.AluOp -> ISA.ScratchAddr -> ISA.ScratchAddr -> ISA.ScratchAddr -> Int -> StepM ()
execVAluLane op dest a b lane = do
  x <- readScratch (scratchPlus a lane)
  y <- readScratch (scratchPlus b lane)
  r <- liftEitherStep (aluEval op x y)
  writeScratch (scratchPlus dest lane) r

execLoad :: ISA.ScratchAddr -> ISA.ScratchAddr -> StepM ()
execLoad dest addrReg = do
  v <- readMemAt addrReg
  writeScratch dest v

execVLoadLane :: ISA.ScratchAddr -> Int -> Int -> StepM ()
execVLoadLane dest base lane = do
  v <- readMemRaw (base + lane)
  writeScratch (scratchPlus dest lane) v

execVLoad :: ISA.ScratchAddr -> ISA.ScratchAddr -> StepM ()
execVLoad dest addrReg = do
  baseW <- readScratch addrReg
  let base = word32ToInt baseW
  forLane8 (execVLoadLane dest base)

execStore :: ISA.ScratchAddr -> ISA.ScratchAddr -> StepM ()
execStore addrReg srcReg = do
  addrW <- readScratch addrReg
  let addr = word32ToInt addrW
  v <- readScratch srcReg
  writeMemRaw addr v

execVStoreLane :: Int -> ISA.ScratchAddr -> Int -> StepM ()
execVStoreLane base src lane = do
  v <- readScratch (scratchPlus src lane)
  writeMemRaw (base + lane) v

execVStore :: ISA.ScratchAddr -> ISA.ScratchAddr -> StepM ()
execVStore addrReg srcReg = do
  baseW <- readScratch addrReg
  let base = word32ToInt baseW
  forLane8 (execVStoreLane base srcReg)

execSelect :: ISA.ScratchAddr -> ISA.ScratchAddr -> ISA.ScratchAddr -> ISA.ScratchAddr -> StepM ()
execSelect dest cond a b = do
  c <- readScratch cond
  va <- readScratch a
  vb <- readScratch b
  writeScratch dest (if isNonZero c then va else vb)

execAddImm :: ISA.ScratchAddr -> ISA.ScratchAddr -> ISA.Imm -> StepM ()
execAddImm dest a (ISA.Imm imm) = do
  x <- readScratch a
  writeScratch dest (x + intToWord32 imm)

execVSelectLane :: ISA.ScratchAddr -> ISA.ScratchAddr -> ISA.ScratchAddr -> ISA.ScratchAddr -> Int -> StepM ()
execVSelectLane dest cond a b lane = do
  c <- readScratch (scratchPlus cond lane)
  va <- readScratch (scratchPlus a lane)
  vb <- readScratch (scratchPlus b lane)
  writeScratch (scratchPlus dest lane) (if isNonZero c then va else vb)

execCondJump :: ISA.ScratchAddr -> ISA.ProgAddr -> StepM ()
execCondJump cond target = do
  c <- readScratch cond
  when (isNonZero c) (setPc target)

execCondJumpRel :: ISA.ScratchAddr -> ISA.Offset -> StepM ()
execCondJumpRel cond (ISA.Offset off) = do
  c <- readScratch cond
  when (isNonZero c) $ do
    StepSnapshot {ssPc} <- ask
    let pc' = progAddrToInt ssPc + 1 + off
    setPc (intToProgAddr pc')

execJumpIndirect :: ISA.ScratchAddr -> StepM ()
execJumpIndirect addrReg = do
  targetW <- readScratch addrReg
  setPc (intToProgAddr (word32ToInt targetW))

execCoreId :: ISA.ScratchAddr -> StepM ()
execCoreId dest = writeScratch dest 0

execHalt :: StepM ()
execHalt = setCoreRunState CoreStopped

execPause :: StepM ()
execPause = do
  StepSnapshot {ssEnablePause} <- ask
  when ssEnablePause (setCoreRunState CorePaused)

execTraceWrite :: ISA.ScratchAddr -> StepM ()
execTraceWrite _ = pure () -- NoOp for now

throwStep :: SimError -> StepM a
throwStep e = lift (lift (Left e))
