module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import BaselineKernel (scalarKernel)
import Canonicalize (canonicalize)
import Vectorize (vectorizeBatch)
import Peephole (combineMulAdd)
import FinalLowering (lowerToMachineOps)
import Bundle (bundleProgram)
import PythonInterop (renderProgramPayload)
import TestSuite (runTests)

defaultRounds :: Int
defaultRounds = 3

defaultBatchSize :: Int
defaultBatchSize = 256

defaultVLen :: Int
defaultVLen = 8

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"]  -> runTests
    [outPath] -> compile outPath defaultRounds defaultBatchSize
    _         -> dieUsage

compile :: FilePath -> Int -> Int -> IO ()
compile outPath rounds batchSize = do
  let fn   = scalarKernel rounds batchSize
      fn'  = combineMulAdd . vectorizeBatch defaultVLen . canonicalize $ fn
      ops  = lowerToMachineOps defaultVLen fn'
      bundles = bundleProgram ops
  writeFile outPath (renderProgramPayload rounds batchSize bundles)
  putStrLn $ "Wrote " ++ show (length bundles) ++ " bundles to " ++ outPath

dieUsage :: IO a
dieUsage = do
  putStrLn "Usage:"
  putStrLn "  compiler <output-path>    Compile and write program"
  putStrLn "  compiler test             Run test suite"
  exitFailure
