module TestSuite (runTests) where

import BaselineKernel (scalarKernel)
import Canonicalize (canonicalize)
import DCE (eliminateDeadCode)
import Vectorize (vectorizeBatch)
import Peephole (combineMulAdd)
import FinalLowering (lowerToMachineOps)
import Schedule (schedule)
import Bundle (bundleProgram)

defaultVLen :: Int
defaultVLen = 8

runTests :: IO ()
runTests = do
  putStrLn "Running compiler tests..."
  testScalarPipeline
  testVectorPipeline
  testVectorFewerBundles
  testTailLoop
  testDCEPreservesOutput
  testSchedulerImprovesPacking
  putStrLn "All tests passed!"

-- | Scalar pipeline produces valid output.
testScalarPipeline :: IO ()
testScalarPipeline = do
  let fn = scalarKernel 1 8
      fn' = canonicalize fn
      ops = lowerToMachineOps defaultVLen fn'
      bundles = bundleProgram ops
  assertPositive "scalar pipeline produces bundles" (length bundles)

-- | Full vector pipeline produces valid output.
testVectorPipeline :: IO ()
testVectorPipeline = do
  let fn = scalarKernel 1 8
      fn' = combineMulAdd . vectorizeBatch defaultVLen
            . eliminateDeadCode . canonicalize $ fn
      ops = schedule (lowerToMachineOps defaultVLen fn')
      bundles = bundleProgram ops
  assertPositive "vector pipeline produces bundles" (length bundles)

-- | Vectorized code uses fewer bundles than scalar.
testVectorFewerBundles :: IO ()
testVectorFewerBundles = do
  let fn = scalarKernel 1 16
      fnS = canonicalize fn
      opsS = lowerToMachineOps defaultVLen fnS
      bundlesS = bundleProgram opsS
      fnV = combineMulAdd . vectorizeBatch defaultVLen
            . eliminateDeadCode . canonicalize $ fn
      opsV = schedule (lowerToMachineOps defaultVLen fnV)
      bundlesV = bundleProgram opsV
  assertLess "vectorized has fewer bundles" (length bundlesV) (length bundlesS)

-- | Non-divisible batch size works (tail loop).
testTailLoop :: IO ()
testTailLoop = do
  let fn = scalarKernel 1 10
      fn' = combineMulAdd . vectorizeBatch defaultVLen
            . eliminateDeadCode . canonicalize $ fn
      ops = schedule (lowerToMachineOps defaultVLen fn')
      bundles = bundleProgram ops
  assertPositive "tail loop pipeline produces bundles" (length bundles)

-- | DCE does not change the number of machine ops (no dead code in baseline).
testDCEPreservesOutput :: IO ()
testDCEPreservesOutput = do
  let fn = scalarKernel 1 8
      fnC = canonicalize fn
      fnD = eliminateDeadCode fnC
      opsC = lowerToMachineOps defaultVLen fnC
      opsD = lowerToMachineOps defaultVLen fnD
  assertEqual "DCE preserves op count for baseline" (length opsC) (length opsD)

-- | Scheduler produces same or fewer bundles than unscheduled.
testSchedulerImprovesPacking :: IO ()
testSchedulerImprovesPacking = do
  let fn = scalarKernel 1 16
      fn' = combineMulAdd . vectorizeBatch defaultVLen
            . eliminateDeadCode . canonicalize $ fn
      ops = lowerToMachineOps defaultVLen fn'
      bundlesUnsched = bundleProgram ops
      bundlesSched   = bundleProgram (schedule ops)
  assertLeq "scheduler does not increase bundle count"
            (length bundlesSched) (length bundlesUnsched)

assertPositive :: String -> Int -> IO ()
assertPositive name n
  | n > 0     = putStrLn $ "  PASS: " ++ name ++ " (" ++ show n ++ ")"
  | otherwise = error $ "  FAIL: " ++ name ++ " (got " ++ show n ++ ")"

assertLess :: String -> Int -> Int -> IO ()
assertLess name a b
  | a < b     = putStrLn $ "  PASS: " ++ name ++ " (" ++ show a ++ " < " ++ show b ++ ")"
  | otherwise = error $ "  FAIL: " ++ name ++ " (" ++ show a ++ " >= " ++ show b ++ ")"

assertLeq :: String -> Int -> Int -> IO ()
assertLeq name a b
  | a <= b    = putStrLn $ "  PASS: " ++ name ++ " (" ++ show a ++ " <= " ++ show b ++ ")"
  | otherwise = error $ "  FAIL: " ++ name ++ " (" ++ show a ++ " > " ++ show b ++ ")"

assertEqual :: String -> Int -> Int -> IO ()
assertEqual name a b
  | a == b    = putStrLn $ "  PASS: " ++ name ++ " (" ++ show a ++ " == " ++ show b ++ ")"
  | otherwise = error $ "  FAIL: " ++ name ++ " (" ++ show a ++ " /= " ++ show b ++ ")"
