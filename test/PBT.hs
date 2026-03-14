{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Control.Monad.Except (runExceptT)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.String (fromString)
import Data.Word (Word32)
import Hedgehog
import ISA qualified
import Sim (SimConfig (..), defaultSimConfig, runSimulator)
import Sim.Types (SimError (..))
import System.Exit (exitFailure, exitSuccess)
import Test.Bridge (PyServer, startPyServer, stopPyServer, queryPyServer)
import Test.Gen (genStraightLine, genWithLoops, genWithValu, genMixed, genAllOps)

main :: IO ()
main = bracket (startPyServer ".") stopPyServer $ \ps -> do
  results <- mapM (\(name, prop) -> do
    putStrLn $ "=== " ++ name ++ " ==="
    checkSequential $ Group (fromString name) [(fromString name, prop)]
    ) [ ("straight-line", prop_straightLine ps),
        ("loops", prop_loops ps),
        ("valu", prop_valu ps),
        ("mixed", prop_mixed ps),
        ("all-ops", prop_allOps ps)
      ]
  if and results then exitSuccess else exitFailure

prop_straightLine :: PyServer -> Property
prop_straightLine ps = withTests 200 $ property $ do
  prog <- forAll genStraightLine
  checkEquiv ps prog

prop_loops :: PyServer -> Property
prop_loops ps = withTests 200 $ property $ do
  prog <- forAll genWithLoops
  checkEquiv ps prog

prop_valu :: PyServer -> Property
prop_valu ps = withTests 200 $ property $ do
  prog <- forAll genWithValu
  checkEquiv ps prog

prop_mixed :: PyServer -> Property
prop_mixed ps = withTests 200 $ property $ do
  prog <- forAll genMixed
  checkEquiv ps prog

prop_allOps :: PyServer -> Property
prop_allOps ps = withTests 200 $ property $ do
  prog <- forAll genAllOps
  checkEquiv ps prog

checkEquiv :: PyServer -> ([ISA.Bundle ()], [Word32]) -> PropertyT IO ()
checkEquiv ps (bundles, mem) = do
  let cfg = defaultSimConfig {scEnableTrace = True}
  hsResult <- evalIO $ runExceptT (runSimulator cfg bundles mem)
  pyResult <- evalIO $ queryPyServer ps bundles mem
  case (hsResult, pyResult) of
    (Right (_, _, hsTrace), Right pyTrace) -> do
      let hs = toList hsTrace
      hs === pyTrace
    (Left hsErr, Left pyErr) -> do
      let hsClass = classifyError hsErr
          pyClass = classifyPyError pyErr
      if hsClass == pyClass
        then label (fromString $ "both error: " ++ hsClass)
        else do
          footnote $
            "Error class mismatch:\n  hs: " ++ hsClass ++ " (" ++ show hsErr ++ ")\n  py: " ++ pyClass ++ " (" ++ pyErr ++ ")"
          failure
    (Left err, Right _) -> do
      footnote $ "Haskell error but Python succeeded: " ++ show err
      failure
    (Right _, Left err) -> do
      footnote $ "Python error but Haskell succeeded: " ++ err
      failure

classifyError :: SimError -> String
classifyError (SimDivideByZero {}) = "div-by-zero"
classifyError (SimMemoryOob {}) = "mem-oob"
classifyError (SimScratchOob {}) = "scratch-oob"
classifyError (SimUnknownPc {}) = "unknown-pc"
classifyError e = show e

classifyPyError :: String -> String
classifyPyError s
  | "ZeroDivision" `isInfixOf` s = "div-by-zero"
  | "division by zero" `isInfixOf` s = "div-by-zero"
  | "index out of range" `isInfixOf` s = "mem-oob"
  | "list index" `isInfixOf` s = "mem-oob"
  | otherwise = s
