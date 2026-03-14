{-# LANGUAGE OverloadedStrings #-}

module Test.Bridge
  ( PyServer,
    startPyServer,
    stopPyServer,
    queryPyServer,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Value (..), eitherDecode', encode, object, toJSON, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import ISA qualified
import Sim.Types (CoreRunState (CorePaused, CoreRunning, CoreStopped), CycleTrace (..))
import System.IO (BufferMode (..), Handle, hFlush, hGetLine, hSetBuffering)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, terminateProcess)

data PyServer = PyServer
  { psIn :: !Handle,
    psOut :: !Handle,
    psProc :: !ProcessHandle
  }

startPyServer :: FilePath -> IO PyServer
startPyServer repoRoot = do
  let script = repoRoot ++ "/scripts/run_ref_server.py"
  (Just hIn, Just hOut, _, ph) <-
    createProcess
      (proc "uv" ["run", script])
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = Inherit
        }
  hSetBuffering hIn LineBuffering
  hSetBuffering hOut LineBuffering
  ready <- hGetLine hOut
  case ready of
    "ready" -> pure ()
    _ -> error $ "Python server unexpected: " ++ ready
  pure PyServer {psIn = hIn, psOut = hOut, psProc = ph}

stopPyServer :: PyServer -> IO ()
stopPyServer ps = do
  hFlush (psIn ps)
  terminateProcess (psProc ps)

-- Send one test case over the pipe, get back the trace.
queryPyServer :: PyServer -> [ISA.Bundle ()] -> [Word32] -> IO (Either String [CycleTrace])
queryPyServer ps bundles mem = do
  let req =
        encode $
          object
            [ "program" .= map bundleToValue bundles,
              "mem" .= map (\w -> toJSON (fromIntegral w :: Int)) mem
            ]
  LBS8.hPutStrLn (psIn ps) req
  hFlush (psIn ps)
  resp <- hGetLine (psOut ps)
  case parseResponse =<< eitherDecode' (LBS8.pack resp) of
    Left err -> pure $ Left err
    Right traces -> pure $ Right $ map toCycleTrace traces

-- Parse {"ok": [...]} or {"error": "msg"}
parseResponse :: Value -> Either String [PyCycleTrace]
parseResponse = parseEither $ withObject "response" $ \o ->
  (o .: "ok") <|> do
    e <- o .: "error"
    fail $ "Python error: " ++ T.unpack e

-- Parsed trace entry from Python's JSON output.
data PyCycleTrace = PyCycleTrace
  { pctCycle :: !Int,
    pctPc :: !Int,
    pctScratchW :: !(IntMap Word32),
    pctMemW :: !(IntMap Word32),
    pctNextPc :: !Int,
    pctRunState :: !Int
  }
  deriving (Show, Eq)

instance FromJSON PyCycleTrace where
  parseJSON = withObject "PyCycleTrace" $ \o -> do
    cyc <- o .: "cycle"
    pc <- o .: "pc"
    swMap <- o .: "scratch_w" :: Parser (Map.Map Text Word32)
    mwMap <- o .: "mem_w" :: Parser (Map.Map Text Word32)
    nextPc <- o .: "next_pc"
    runSt <- o .: "run_state"
    let toIntMap m = IM.fromList [(read (T.unpack k), v) | (k, v) <- Map.toList m]
    pure
      PyCycleTrace
        { pctCycle = cyc,
          pctPc = pc,
          pctScratchW = toIntMap swMap,
          pctMemW = toIntMap mwMap,
          pctNextPc = nextPc,
          pctRunState = runSt
        }

toCycleTrace :: PyCycleTrace -> CycleTrace
toCycleTrace pct =
  CycleTrace
    { ctCycle = pctCycle pct,
      ctPc = ISA.ProgAddr (pctPc pct),
      ctScratchW = pctScratchW pct,
      ctMemW = pctMemW pct,
      ctNextPc = ISA.ProgAddr (pctNextPc pct),
      ctNextRun = case pctRunState pct of
        1 -> CoreRunning
        2 -> CorePaused
        _ -> CoreStopped
    }

bundleToValue :: ISA.Bundle () -> Value
bundleToValue b =
  object $
    filter
      (not . isEmptyArray . snd)
      [ "alu" .= map aluSlotToValue (ISA.aluSlots b),
        "valu" .= map valuSlotToValue (ISA.valuSlots b),
        "load" .= map loadSlotToValue (ISA.loadSlots b),
        "store" .= map storeSlotToValue (ISA.storeSlots b),
        "flow" .= map flowSlotToValue (ISA.flowSlots b)
      ]
  where
    isEmptyArray (Array a) = null a
    isEmptyArray _ = False

mkSlot :: String -> [Value] -> Value
mkSlot op args = toJSON (toJSON op : args)

sa :: ISA.ScratchAddr -> Value
sa (ISA.ScratchAddr n) = toJSON n

pa :: ISA.ProgAddr -> Value
pa (ISA.ProgAddr n) = toJSON n

aluSlotToValue :: ISA.AluSlot -> Value
aluSlotToValue (ISA.Alu op d a b) =
  mkSlot (ISA.renderAluOp op) [sa d, sa a, sa b]

valuSlotToValue :: ISA.ValuSlot -> Value
valuSlotToValue v = case v of
  ISA.VBroadcast d s -> mkSlot "vbroadcast" [sa d, sa s]
  ISA.MultiplyAdd d a b c -> mkSlot "multiply_add" [sa d, sa a, sa b, sa c]
  ISA.VAlu op d a b -> mkSlot (ISA.renderAluOp op) [sa d, sa a, sa b]

loadSlotToValue :: ISA.LoadSlot -> Value
loadSlotToValue l = case l of
  ISA.Load d a -> mkSlot "load" [sa d, sa a]
  ISA.LoadOffset d a (ISA.Offset o) -> mkSlot "load_offset" [sa d, sa a, toJSON o]
  ISA.VLoad d a -> mkSlot "vload" [sa d, sa a]
  ISA.Const d w -> mkSlot "const" [sa d, toJSON (fromIntegral w :: Int)]

storeSlotToValue :: ISA.StoreSlot -> Value
storeSlotToValue s = case s of
  ISA.Store a v -> mkSlot "store" [sa a, sa v]
  ISA.VStore a v -> mkSlot "vstore" [sa a, sa v]

flowSlotToValue :: ISA.FlowSlot -> Value
flowSlotToValue f = case f of
  ISA.Select d c a b -> mkSlot "select" [sa d, sa c, sa a, sa b]
  ISA.AddImm d a (ISA.Imm i) -> mkSlot "add_imm" [sa d, sa a, toJSON i]
  ISA.VSelect d c a b -> mkSlot "vselect" [sa d, sa c, sa a, sa b]
  ISA.Halt -> mkSlot "halt" []
  ISA.Pause -> mkSlot "pause" []
  ISA.TraceWrite v -> mkSlot "trace_write" [sa v]
  ISA.CondJump c a -> mkSlot "cond_jump" [sa c, pa a]
  ISA.CondJumpRel c (ISA.Offset o) -> mkSlot "cond_jump_rel" [sa c, toJSON o]
  ISA.Jump a -> mkSlot "jump" [pa a]
  ISA.JumpIndirect a -> mkSlot "jump_indirect" [sa a]
  ISA.CoreId d -> mkSlot "coreid" [sa d]
