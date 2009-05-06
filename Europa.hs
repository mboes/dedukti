module Main where

import System.Environment
import Europa.Config as Config
import Europa.EuM
import Text.PrettyPrint.Leijen
import System.Exit
import System.IO
import Europa.Pretty
import Europa.Parser


printUsage = do
  self <- runEuM $ select Config.imageName
  putDoc $ text self <+> text "[OPTION]..." <+> text "FILE"

main = do
  args <- getArgs
  (filename, handle) <-
      case args of
        [path] -> do h <- openFile path ReadMode
                     return (path, h)
        _ -> do printUsage
                exitFailure
  input <- hGetContents handle
  case parse filename input of
    Left e -> error (show e)
    Right (decls, rules) -> do
         putStrLn "Rules:\n"
         putStrLn . show . pretty $ rules
         putStrLn "\nDeclarations:\n"
         putStrLn . show . pretty $ decls
