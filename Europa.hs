module Main where

import System.Environment
import System.Exit
import System.IO
import Europa.Pretty
import Europa.Parser


printUsage = putStrLn "europa [FILE]" >> exitFailure

main = do
  args <- getArgs
  (filename, handle) <-
      case args of
        [path] -> do h <- openFile path ReadMode; return (path, h)
        _ -> printUsage
  input <- hGetContents handle
  case parse filename input of
    Left e -> error (show e)
    Right (decls, rules) -> do
         putStrLn "Rules:\n"
         putStrLn . show . pretty $ rules
         putStrLn "\nDeclarations:\n"
         putStrLn . show . pretty $ decls
