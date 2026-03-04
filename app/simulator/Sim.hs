module Sim
  ( SimConfig (..),
    CoreRunState (..),
    CoreState (..),
    MachineState (..),
    StepSnapshot (..),
    PendingWrites (..),
    SimError (..),
    SimM,
    StepM,
    runSimulator,
    defaultSimConfig,
    initMachineState,
    finalMemoryImage,
    readMemFile,
    writeMemFile,
    runProgram,
    runUntilStop,
    stepCore,
    fetchBundle,
    bundleCountsAsCycle,
    buildSnapshot,
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
    readScratch,
    readMemAt,
    readMemRaw,
    writeScratch,
    writeMemAt,
    writeMemRaw,
    setPc,
    setCoreRunState,
    ensureScratchInBounds,
    ensureMemInBounds,
    word32ToInt,
    intToWord32,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (MonadTrans (lift), ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (StateT, get, modify', put, runStateT)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.IntMap qualified as IM
import Data.IntMap.Strict (IntMap)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word8)
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

-- Top-level API

runSimulator :: SimConfig -> [ISA.Bundle ()] -> [Word32] -> ExceptT SimError IO ([Word32], Maybe Int)
runSimulator cfg bundles mem0 = do
  let st0 = initMachineState cfg mem0
  (_, st1) <- runStateT (runReaderT (runProgram bundles) cfg) st0
  pure (finalMemoryImage st1, Just (msCycle st1))

defaultSimConfig :: SimConfig
defaultSimConfig =
  SimConfig
    { scMachineConfig = ISA.defaultMachineConfig,
      scScratchSize = 1536,
      scEnablePause = False
    }

-- Initialization and conversion helpers

initMachineState :: SimConfig -> [Word32] -> MachineState
initMachineState cfg memImage =
  MachineState
    { msCore =
        CoreState
          { csId = 0,
            csPc = ISA.ProgAddr 0,
            csRunState = CoreRunning,
            csScratch = denseMap (replicate (scScratchSize cfg) 0),
            csTraceBuf = []
          },
      msMem = denseMap memImage,
      msCycle = 0
    }
  where
    denseMap xs = IM.fromDistinctAscList (zip [0 ..] xs)

finalMemoryImage :: MachineState -> [Word32]
finalMemoryImage ms =
  map (\ix -> IM.findWithDefault 0 ix mem) [0 .. IM.size mem - 1]
  where
    mem = msMem ms

readMemFile :: FilePath -> IO [Word32]
readMemFile path = do
  bs <- BS.readFile path
  let n = BS.length bs
  when
    (n `mod` 4 /= 0)
    (ioError (userError ("Memory file size must be a multiple of 4 bytes, got " ++ show n)))
  pure [decodeWord32LEAt bs i | i <- [0, 4 .. n - 4]]

writeMemFile :: FilePath -> [Word32] -> IO ()
writeMemFile path ws =
  BS.writeFile path (BS.pack (concatMap encodeWord32LE ws))

-- Main execution loop

runProgram :: [ISA.Bundle ()] -> SimM ()
runProgram bundles = do
  modify' $ \ms ->
    let core = msCore ms
        core' = case csRunState core of
          CorePaused -> core {csRunState = CoreRunning}
          _ -> core
     in ms {msCore = core'}
  runUntilStop bundles

runUntilStop :: [ISA.Bundle ()] -> SimM ()
runUntilStop bundles = do
  progressed <- stepCore bundles
  when progressed (runUntilStop bundles)

stepCore :: [ISA.Bundle ()] -> SimM Bool
stepCore bundles = do
  ms <- get
  let core = msCore ms
      pc = csPc core
      pcIx = progAddrToInt pc
      nBundles = length bundles
  case csRunState core of
    CoreRunning
      | pcIx >= nBundles -> do
          put ms {msCore = core {csRunState = CoreStopped}}
          pure False
      | otherwise ->
          case fetchBundle bundles pc of
            Left err -> lift (lift (throwError err))
            Right bundle -> do
              executeBundle bundle
              when (bundleCountsAsCycle bundle) $
                modify' (\s -> s {msCycle = msCycle s + 1})
              pure True
    _ -> pure False

fetchBundle ::
  [ISA.Bundle ()] ->
  ISA.ProgAddr ->
  Either SimError (ISA.Bundle ())
fetchBundle bundles pc
  | ix < 0 = Left (SimUnknownPc ix)
  | ix >= length bundles = Left (SimUnknownPc ix)
  | otherwise = Right (bundles !! ix)
  where
    ix = progAddrToInt pc

bundleCountsAsCycle :: ISA.Bundle k -> Bool
bundleCountsAsCycle bundle =
  not
    ( null (ISA.aluSlots bundle)
        && null (ISA.valuSlots bundle)
        && null (ISA.loadSlots bundle)
        && null (ISA.storeSlots bundle)
        && null (ISA.flowSlots bundle)
    )

-- Cycle semantics (snapshot + deferred writes)

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

executeBundle :: ISA.Bundle () -> SimM ()
executeBundle bundle = do
  cfg <- ask
  ms <- get
  let snap = buildSnapshot cfg ms
  case runStateT (runReaderT (executeBundleSlots bundle) snap) emptyPendingWrites of
    Left err -> lift (lift (throwError err))
    Right (_, pw) -> commitPendingWrites pw

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

-- Evaluators and exectuion helpers

scratchPlus :: ISA.ScratchAddr -> Int -> ISA.ScratchAddr
scratchPlus (ISA.ScratchAddr base) off = ISA.ScratchAddr (base + off)

progAddrToInt :: ISA.ProgAddr -> Int
progAddrToInt (ISA.ProgAddr n) = n

intToProgAddr :: Int -> ISA.ProgAddr
intToProgAddr = ISA.ProgAddr

isNonZero :: Word32 -> Bool
isNonZero w = w /= 0

aluEval :: ISA.AluOp -> Word32 -> Word32 -> Either SimError Word32
aluEval op a b = case op of -- Some issue here?
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

-- Validation and small utilities

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

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

intToWord32 :: Int -> Word32
intToWord32 n = fromIntegral (toInteger n `mod` (2 ^ (32 :: Integer)))

decodeWord32LEAt :: BS.ByteString -> Int -> Word32
decodeWord32LEAt bs i =
  fromIntegral (byte 0)
    .|. (fromIntegral (byte 1) `shiftL` 8)
    .|. (fromIntegral (byte 2) `shiftL` 16)
    .|. (fromIntegral (byte 3) `shiftL` 24)
  where
    byte off = BS.index bs (i + off)

encodeWord32LE :: Word32 -> [Word8]
encodeWord32LE w =
  [ fromIntegral w,
    fromIntegral (w `shiftR` 8),
    fromIntegral (w `shiftR` 16),
    fromIntegral (w `shiftR` 24)
  ]
