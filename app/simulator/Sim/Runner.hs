module Sim.Runner
  ( runSimulator,
    defaultSimConfig,
    initMachineState,
    finalMemoryImage,
    runProgram,
    runUntilStop,
    stepCore,
    fetchBundle,
    bundleCountsAsCycle,
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (MonadTrans (lift), runReaderT)
import Control.Monad.State.Strict (get, modify', put, runStateT)
import Data.IntMap qualified as IM
import Data.Word (Word32)
import ISA qualified
import Sim.Exec (executeBundle)
import Sim.Types

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

progAddrToInt :: ISA.ProgAddr -> Int
progAddrToInt (ISA.ProgAddr n) = n
