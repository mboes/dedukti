module Main where

import System.Environment
import Europa.Config as Config
import Europa.EuM
import Europa.Pretty
import Europa.Parser
import Europa.Module
import Europa.Driver.Batch
import Europa.Driver.Compile
import Text.PrettyPrint.Leijen
import Control.Monad (when)
import System.Console.GetOpt
import System.Exit
import System.IO


data Flag = Make
            deriving (Eq, Ord, Show)

options = [ Option [] ["make"] (NoArg Make)
                       "Build FILE and all its dependencies in one go." ]

data Usage = Help | WrongUsage

printUsage format = do
  self <- parameter Config.imageName
  let header = show $ text "Usage:"
               <+> text self <+> text "[OPTION]..." <+> text "FILE"
  case format of
    Help -> io $ putStrLn (usageInfo header options) >> exitSuccess
    WrongUsage -> io $ hPutStrLn stderr header >> exitFailure

main = do
  args <- getArgs
  let (opts, args, errs) = getOpt Permute options args
  when (length args /= 1 || not (null errs)) $ do
         hPutDoc stderr (vsep (map text errs))
         exitFailure
  let mod = moduleFromPath (head args)
  runEuM Config.defaultConfig $
         case opts of
           _ | Make `elem` opts -> make [mod]
             | otherwise -> compile mod

