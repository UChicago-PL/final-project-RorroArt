module Main where

import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Data.Foldable (toList)
import Parser (ProgramTyped(..), parseProgramPayloadFileTypedWith)
import Sim
  ( SimConfig(..)
  , SimError(..)
  , defaultSimConfig
  , readMemFile
  , renderCycleTrace
  , runSimulator
  , writeMemFile
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stderr)

data CliArgs = CliArgs
  { caProgramPath :: !FilePath
  , caMemPath :: !FilePath
  , caTrace :: !Bool
  , caStreamTrace :: !Bool
  }

main :: IO ()
main = do
  args <- getArgs
  case parseCliArgs args of
    Left err -> do
      hPutStrLn stderr err
      printUsage
      exitFailure
    Right cli -> runCli cli

runCli :: CliArgs -> IO ()
runCli cli = do
  let cfg = defaultSimConfig { scEnableTrace = caTrace cli, scStreamTrace = caStreamTrace cli }
  parsed <- parseProgramPayloadFileTypedWith (scMachineConfig cfg) (caProgramPath cli)
  case parsed of
    Left err -> failWith ("Parse failed: " ++ err)
    Right ProgramTyped {ptProgram} -> do
      mem0 <- readMemFile (caMemPath cli)
      res <- runExceptT (runSimulator cfg ptProgram mem0)
      case res of
        Left simErr ->
          failWith ("Simulation failed: " ++ renderSimError simErr)
        Right (memOut, cycleCount, trace) -> do
          writeMemFile (caMemPath cli) memOut
          when (caTrace cli) $
            hPutStr stderr (concatMap (\ct -> renderCycleTrace ct ++ "\n") (toList trace))
          putStrLn "---"
          putStrLn "Program Executed Successfully!!"
          putStrLn ("Cycle Count = " ++ show cycleCount)

parseCliArgs :: [String] -> Either String CliArgs
parseCliArgs = go (CliArgs "" "" False False)
  where
    go cli ("--trace" : rest) = go cli { caTrace = True } rest
    go cli ("--sm-trace" : rest) = go cli { caStreamTrace = True } rest
    go cli [programPath, "-m", memPath] = Right cli { caProgramPath = programPath, caMemPath = memPath }
    go cli [programPath, "--mem", memPath] = Right cli { caProgramPath = programPath, caMemPath = memPath }
    go _ _ = Left "Invalid arguments."

printUsage :: IO ()
printUsage = do
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr "  simulator [--trace] [--sm-trace] <program.bundle> -m <mem.bin>"

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitFailure

renderSimError :: SimError -> String
renderSimError err = case err of
  SimParseError msg ->
    "parse error: " ++ msg
  SimInvalidProgram msg ->
    "invalid program: " ++ msg
  SimScratchOob i ->
    "scratch out of bounds: " ++ show i
  SimMemoryOob i ->
    "memory out of bounds: " ++ show i
  SimUnknownPc i ->
    "unknown PC: " ++ show i
  SimDivideByZero op a b ->
    "divide/mod by zero in " ++ show op ++ " with lhs=" ++ show a ++ " rhs=" ++ show b
  SimUnsupported msg ->
    "unsupported: " ++ msg
