module Main where

import System.Environment
import qualified Europa.Config as Config
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


data Flag = FlagMake | FlagHelp | FlagVerbose | FlagVeryVerbose
            deriving (Eq, Ord, Show)

options = [ Option [] ["make"] (NoArg FlagMake)
                       "Build MODULE and all its dependencies in one go."
          , Option ['v'] [] (OptArg verb "v")
                       "Be verbose. -vv to be even more verbose."
          , Option ['h'] ["help"] (NoArg FlagHelp) "This usage information." ]
    where verb Nothing = FlagVerbose
          verb (Just "v") = FlagVeryVerbose
          verb _ = error "Unrecognized verbosity level."

data Usage = Long | Short

printUsage format = do
  self <- parameter Config.imageName
  let header = show $ text "Usage:" <+>
               (text self <+> text "[OPTION]..." <+> text "MODULE")
               <$> text "Options:"
  case format of
    Long -> io $ putStrLn (usageInfo header options)
    Short -> io $ hPutStrLn stderr header

bailout = printUsage Short >> io exitFailure

initializeConfiguration = foldr aux Config.defaultConfig
    where aux FlagVerbose c     = c { Config.verbosity = Verbose }
          aux FlagVeryVerbose c = c { Config.verbosity = Debug }
          aux _ c               = c

main = do
  args <- getArgs
  let (opts, files, errs) = getOpt RequireOrder options args
  when (not (null errs)) $ do
         hPutDoc stderr (vsep (map text errs))
         exitFailure
  runEuM (initializeConfiguration opts) $
         case undefined of
           _ | FlagHelp `elem` opts -> printUsage Long
           _ | FlagMake `elem` opts -> do
                     unless (length files == 1) bailout
                     make [moduleFromPath (head files)]
             | otherwise -> do
                     unless (length files == 1) bailout
                     compile (moduleFromPath (head files))

