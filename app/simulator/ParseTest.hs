module Main where

import Parser (parseProgramPayloadFileTyped, ProgramTyped(..))
import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  putStrLn $ "Parsing: " ++ path
  result <- parseProgramPayloadFileTyped path
  case result of
    Left err -> putStrLn $ "FAIL: " ++ err
    Right pt -> putStrLn $ "OK: " ++ show (length (ptProgram pt)) ++ " bundles"
