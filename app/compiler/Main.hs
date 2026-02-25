module Main where

import BaselineKernelFinalLowering (lowerBaselineKernelFinalIR)
import qualified BaselineKernelFinalIR as FinalBaseline
import PythonInterop (renderProgramPayload)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["export-baseline", outPath] ->
      writePayload outPath FinalBaseline.baselineRounds FinalBaseline.baselineBatchSize
    ["export-baseline", outPath, roundsStr, batchStr] ->
      case (readMaybe roundsStr, readMaybe batchStr) of
        (Just rounds, Just batchSize) -> writePayload outPath rounds batchSize
        _ -> dieUsage
    ["check-ir-lowering"] ->
      checkIrLowering FinalBaseline.baselineRounds FinalBaseline.baselineBatchSize
    ["check-ir-lowering", roundsStr, batchStr] ->
      case (readMaybe roundsStr, readMaybe batchStr) of
        (Just rounds, Just batchSize) -> checkIrLowering rounds batchSize
        _ -> dieUsage
    _ -> dieUsage
  where
    writePayload outPath rounds batchSize = do
      let kernel = FinalBaseline.buildBaselineKernelFinalIR rounds batchSize
      case lowerBaselineKernelFinalIR kernel of
        Left err -> do
          putStrLn $ "FinalIR lowering failed: " ++ err
          exitFailure
        Right program ->
          writeFile outPath (renderProgramPayload rounds batchSize program)

    checkIrLowering rounds batchSize = do
      let kernel = FinalBaseline.buildBaselineKernelFinalIR rounds batchSize
      case lowerBaselineKernelFinalIR kernel of
        Left err -> do
          putStrLn $ "FinalIR lowering check: FAIL (" ++ err ++ ")"
          exitFailure
        Right lowered -> do
          putStrLn "FinalIR lowering check: PASS"
          putStrLn $ "Lowered bundle count: " ++ show (length lowered)

dieUsage :: IO a
dieUsage = do
  putStrLn "Usage:"
  putStrLn "  compiler export-baseline <payload-path>"
  putStrLn "  compiler export-baseline <payload-path> <rounds> <batch-size>"
  putStrLn "  compiler check-ir-lowering"
  putStrLn "  compiler check-ir-lowering <rounds> <batch-size>"
  exitFailure
