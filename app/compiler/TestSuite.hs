module TestSuite (runTests) where

import BaselineKernel (scalarKernel)
import Canonicalize (canonicalize)
import Vectorize (vectorizeBatch)
import Peephole (combineMulAdd)
import FinalLowering (lowerToMachineOps)
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
  putStrLn "All tests passed!"

testScalarPipeline :: IO ()
testScalarPipeline = do
  let fn = scalarKernel 1 8
      fn' = canonicalize fn
      ops = lowerToMachineOps defaultVLen fn'
      bundles = bundleProgram ops
  assertPositive "scalar pipeline produces bundles" (length bundles)

testVectorPipeline :: IO ()
testVectorPipeline = do
  let fn = scalarKernel 1 8
      fn' = combineMulAdd . vectorizeBatch defaultVLen . canonicalize $ fn
      ops = lowerToMachineOps defaultVLen fn'
      bundles = bundleProgram ops
  assertPositive "vector pipeline produces bundles" (length bundles)

testVectorFewerBundles :: IO ()
testVectorFewerBundles = do
  let fn = scalarKernel 1 16
      fnS = canonicalize fn
      opsS = lowerToMachineOps defaultVLen fnS
      bundlesS = bundleProgram opsS
      fnV = combineMulAdd . vectorizeBatch defaultVLen . canonicalize $ fn
      opsV = lowerToMachineOps defaultVLen fnV
      bundlesV = bundleProgram opsV
  assertLess "vectorized has fewer bundles" (length bundlesV) (length bundlesS)

testTailLoop :: IO ()
testTailLoop = do
  -- batchSize=10 not divisible by 8: should still produce output
  let fn = scalarKernel 1 10
      fn' = combineMulAdd . vectorizeBatch defaultVLen . canonicalize $ fn
      ops = lowerToMachineOps defaultVLen fn'
      bundles = bundleProgram ops
  assertPositive "tail loop pipeline produces bundles" (length bundles)

assertPositive :: String -> Int -> IO ()
assertPositive name n
  | n > 0     = putStrLn $ "  PASS: " ++ name ++ " (" ++ show n ++ ")"
  | otherwise = error $ "  FAIL: " ++ name ++ " (got " ++ show n ++ ")"

assertLess :: String -> Int -> Int -> IO ()
assertLess name a b
  | a < b     = putStrLn $ "  PASS: " ++ name ++ " (" ++ show a ++ " < " ++ show b ++ ")"
  | otherwise = error $ "  FAIL: " ++ name ++ " (" ++ show a ++ " >= " ++ show b ++ ")"
