module Main where

import BaselineDebugPolicy (baselineDebugPolicy)
import qualified BaselineKernelBatchingFinalIR as BatchingBaseline
import BaselineKernelFinalLowering (lowerBaselineKernelFinalIR)
import qualified BaselineKernelFinalIR as FinalBaseline
import BatchVectorizeFinalIR (BatchConfig(..), batchVectorizeKernel, defaultBatchConfig)
import BatchvecProgramSuite (runBatchvecSuite)
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
    ["export-baseline-batchvec", outPath] ->
      writePayloadBatchvec outPath BatchingBaseline.baselineRounds BatchingBaseline.baselineBatchSize
    ["export-baseline-batchvec", outPath, roundsStr, batchStr] ->
      case (readMaybe roundsStr, readMaybe batchStr) of
        (Just rounds, Just batchSize) -> writePayloadBatchvec outPath rounds batchSize
        _ -> dieUsage
    ["check-ir-lowering"] ->
      checkIrLowering FinalBaseline.baselineRounds FinalBaseline.baselineBatchSize
    ["check-ir-lowering", roundsStr, batchStr] ->
      case (readMaybe roundsStr, readMaybe batchStr) of
        (Just rounds, Just batchSize) -> checkIrLowering rounds batchSize
        _ -> dieUsage
    ["check-batchvec-lowering"] ->
      checkBatchvecLowering BatchingBaseline.baselineRounds BatchingBaseline.baselineBatchSize
    ["check-batchvec-lowering", roundsStr, batchStr] ->
      case (readMaybe roundsStr, readMaybe batchStr) of
        (Just rounds, Just batchSize) -> checkBatchvecLowering rounds batchSize
        _ -> dieUsage
    ["batchvec-stats"] ->
      printBatchvecStats BatchingBaseline.baselineRounds BatchingBaseline.baselineBatchSize
    ["batchvec-stats", roundsStr, batchStr] ->
      case (readMaybe roundsStr, readMaybe batchStr) of
        (Just rounds, Just batchSize) -> printBatchvecStats rounds batchSize
        _ -> dieUsage
    ["batchvec-suite"] ->
      runBatchvecSuite
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

    writePayloadBatchvec outPath rounds batchSize = do
      result <- vectorizeBatchingKernel rounds batchSize
      case result of
        Left err -> do
          putStrLn $ "Batchvec failed: " ++ err
          exitFailure
        Right (kernelVec, _stats) ->
          case lowerBaselineKernelFinalIR kernelVec of
            Left err -> do
              putStrLn $ "FinalIR batchvec lowering failed: " ++ err
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

    checkBatchvecLowering rounds batchSize = do
      let kernel = BatchingBaseline.buildBaselineKernelBatchingFinalIR rounds batchSize
      case lowerBaselineKernelFinalIR kernel of
        Left err -> do
          putStrLn $ "Baseline batching kernel lowering check: FAIL (" ++ err ++ ")"
          exitFailure
        Right lowered -> do
          putStrLn "Baseline batching kernel lowering check: PASS"
          putStrLn $ "Baseline lowered bundle count: " ++ show (length lowered)
      result <- vectorizeBatchingKernel rounds batchSize
      case result of
        Left err -> do
          putStrLn $ "Batchvec failed: " ++ err
          exitFailure
        Right (kernelVec, stats) ->
          case lowerBaselineKernelFinalIR kernelVec of
            Left err -> do
              putStrLn $ "Batchvec lowering check: FAIL (" ++ err ++ ")"
              exitFailure
            Right loweredVec -> do
              putStrLn $ "Batchvec stats: " ++ show stats
              putStrLn "Batchvec lowering check: PASS"
              putStrLn $ "Batchvec lowered bundle count: " ++ show (length loweredVec)

    printBatchvecStats rounds batchSize = do
      result <- vectorizeBatchingKernel rounds batchSize
      case result of
        Left err -> do
          putStrLn $ "Batchvec failed: " ++ err
          exitFailure
        Right (kernelVec, stats) -> do
          putStrLn $ "Batchvec stats: " ++ show stats
          case lowerBaselineKernelFinalIR kernelVec of
            Left err -> do
              putStrLn $ "Lowering batchvec kernel failed: " ++ err
              exitFailure
            Right lowered ->
              putStrLn $ "Batchvec lowered bundle count: " ++ show (length lowered)

    vectorizeBatchingKernel rounds batchSize = do
      let kernel = BatchingBaseline.buildBaselineKernelBatchingFinalIR rounds batchSize
          cfg :: BatchConfig FinalBaseline.BaselineDebugKeyRef
          cfg = defaultBatchConfig { bcDebugPolicy = baselineDebugPolicy }
      pure (batchVectorizeKernel cfg kernel)

dieUsage :: IO a
dieUsage = do
  putStrLn "Usage:"
  putStrLn "  compiler export-baseline <payload-path>"
  putStrLn "  compiler export-baseline <payload-path> <rounds> <batch-size>"
  putStrLn "  compiler export-baseline-batchvec <payload-path>"
  putStrLn "  compiler export-baseline-batchvec <payload-path> <rounds> <batch-size>"
  putStrLn "  compiler check-ir-lowering"
  putStrLn "  compiler check-ir-lowering <rounds> <batch-size>"
  putStrLn "  compiler check-batchvec-lowering"
  putStrLn "  compiler check-batchvec-lowering <rounds> <batch-size>"
  putStrLn "  compiler batchvec-stats"
  putStrLn "  compiler batchvec-stats <rounds> <batch-size>"
  putStrLn "  compiler batchvec-suite"
  exitFailure
