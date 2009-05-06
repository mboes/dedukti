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
import Control.Monad (unless, when)
import System.Console.GetOpt
import System.Exit
import System.IO


data Flag = Make | Help | Verbose | VeryVerbose
            deriving (Eq, Ord, Show)

options = [ Option [] ["make"] (NoArg Make)
                       "Build MODULE and all its dependencies in one go."
          , Option ['h'] ["help"] (NoArg Help) "This usage information."
          , Option ['v'] [] (OptArg verb "v")
                       "Be verbose. -vv to be even more verbose." ]
    where verb Nothing = Verbose
          verb (Just "v") = VeryVerbose
          verb _ = error "Unrecognized verbosity level."

data Usage = Long | Short

printUsage format = do
  self <- parameter Config.imageName
  let header = show $ text "Usage:" <+>
               (text self <+> text "[OPTION]..." <+> text "MODULE")
  case format of
    Long -> io $ putStrLn (usageInfo header options)
    Short -> io $ hPutStrLn stderr header

bailout = printUsage Short >> io exitFailure

main = do
  args <- getArgs
  let (opts, files, errs) = getOpt RequireOrder options args
  when (not (null errs)) $ do
         hPutDoc stderr (vsep (map text errs))
         exitFailure
  runEuM Config.defaultConfig $
         case opts of
           _ | Help `elem` opts -> printUsage Long
           _ | Make `elem` opts -> do
                     unless (length files == 1) bailout
                     make [moduleFromPath (head files)]
             | otherwise -> do
                     unless (length files == 1) bailout
                     compile (moduleFromPath (head files))

