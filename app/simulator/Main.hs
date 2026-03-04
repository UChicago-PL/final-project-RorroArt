module Main where

import Control.Monad.Except (runExceptT)
import Parser (ProgramTyped(..), parseProgramPayloadFileTypedWith)
import Sim
  ( SimConfig(..)
  , SimError(..)
  , defaultSimConfig
  , readMemFile
  , runSimulator
  , writeMemFile
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data CliArgs = CliArgs
  { caProgramPath :: !FilePath
  , caMemPath :: !FilePath
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
  let cfg = defaultSimConfig
  parsed <- parseProgramPayloadFileTypedWith (scMachineConfig cfg) (caProgramPath cli)
  case parsed of
    Left err -> failWith ("Parse failed: " ++ err)
    Right ProgramTyped {ptProgram} -> do
      mem0 <- readMemFile (caMemPath cli)
      res <- runExceptT (runSimulator cfg ptProgram mem0)
      case res of
        Left simErr ->
          failWith ("Simulation failed: " ++ renderSimError simErr)
        Right (memOut, cycleMay) -> do
          writeMemFile (caMemPath cli) memOut
          case cycleMay of
            Just n -> putStrLn ("{\"cycle\":" ++ show n ++ "}")
            Nothing -> pure ()

parseCliArgs :: [String] -> Either String CliArgs
parseCliArgs args = case args of
  [programPath, "-m", memPath] ->
    Right (CliArgs programPath memPath)
  [programPath, "--mem", memPath] ->
    Right (CliArgs programPath memPath)
  _ ->
    Left "Invalid arguments."

printUsage :: IO ()
printUsage = do
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr "  simulator <program.bundle> -m <mem.bin>"
  hPutStrLn stderr "  simulator <program.bundle> --mem <mem.bin>"

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
