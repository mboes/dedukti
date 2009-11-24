-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- This module is the entry point for europa. Based on the command line
-- arguments the appropriate driver is invoked. Everything is coordinated by
-- the driver. This module is also the place where global configuration data
-- is initialized.

module Main where

import System.Environment
import qualified Dedukti.Config as Config
import Dedukti.DkM
import Dedukti.Module
import Dedukti.Driver.Batch
import Dedukti.Driver.Compile
import Text.PrettyPrint.Leijen
import Control.Monad (unless, when)
import System.Console.GetOpt
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy.Char8 as B


data Flag = FlagMake | FlagHelp | FlagVersion | FlagVerbose | FlagVeryVerbose
            deriving (Eq, Ord, Show)

options = [ Option [] ["make"] (NoArg FlagMake)
                       "Build MODULE and all its dependencies in one go."
          , Option ['v'] [] (OptArg verb "v")
                       "Be verbose. -vv to be even more verbose."
          , Option ['h'] ["help"] (NoArg FlagHelp) "This usage information."
          , Option [] ["version"] (NoArg FlagVersion) "Output version information then exit." ]
    where verb Nothing = FlagVerbose
          verb (Just "v") = FlagVeryVerbose
          verb _ = error "Unrecognized verbosity level."

printUsage = do
  self <- parameter Config.imageName
  let header = show $ text "Usage:" <+>
               (text self <+> text "[OPTION]..." <+> text "MODULE")
  io $ hPutStrLn stderr header

printHelp = do
  self <- parameter Config.imageName
  let header = show $ text "Usage:" <+>
               (text self <+> text "[OPTION]..." <+> text "MODULE")
               <$> text "Options:"
  io $ putStrLn (usageInfo header options)

bailout = printUsage >> io exitFailure

printVersion = do
  self <- parameter Config.imageName
  version <- parameter Config.version
  io $ B.putStrLn $ B.pack $ flip displayS "" $ renderPretty 0.70 100 $
     text "Dedukti" <+> text version <> line <> line <>
     text "Copyright (c) 2009 CNRS - École Polytechnique - INRIA." <> line <> line <>
     fillText "You may redistribute copies of Dedukti under the terms of \
              \the GNU General Public License. For more information about \
              \these matters, see the file named COPYING."

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
  (`runDkM` initializeConfiguration opts) $
         case undefined of
           _ | FlagHelp `elem` opts -> printHelp
             | FlagVersion `elem` opts -> printVersion
           _ | FlagMake `elem` opts -> do
                     unless (length files == 1) bailout
                     make [moduleFromPath (head files)]
             | otherwise -> do
                     unless (length files == 1) bailout
                     compile (moduleFromPath (head files))
