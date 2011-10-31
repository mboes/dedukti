-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- This module is the entry point for Dedukti. Based on the command line
-- arguments the appropriate driver is invoked. Everything is coordinated by
-- the driver. This module is also the place where global configuration data
-- is initialized.

module Main where

import qualified Dedukti.Config as Config
import Dedukti.DkM
import Dedukti.Module
import Dedukti.Driver.Batch
import Dedukti.Driver.Compile
import System.Console.Option
import Text.PrettyPrint.Leijen
import System.Environment
import System.Exit
import System.IO (stderr)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Paths_dedukti as Cabal (version)
import Data.Version


data Flag = FlagMake
          | FlagJobs Int
          | FlagFormat Config.Format
          | FlagCodeGen Config.CodeGen
          | FlagHelp | FlagVersion
          | FlagVerbose | FlagVeryVerbose
            deriving (Eq, Ord, Show)

flagDescriptions =
  [ ([Nullary "--make" FlagMake],
     "Build MODULE and all its dependencies in one go.")
  , ([Unary "-jN" (FlagJobs . read)],
     "Perform N jobs at once.")
  , ([Nullary "-fexternal" (FlagFormat Config.External)],
     "Force recognizing input as (human-readable) external format.")
  , ([Nullary "-fprefix-notation" (FlagFormat Config.Prefix)],
     "Force recognizing input as (fast) prefix format.")
  , ([Nullary "-flua" (FlagCodeGen Config.Lua)],
     "Compile to Lua code.")
  , ([Unary "-v[v]" $ \arg -> if null arg then FlagVerbose else FlagVeryVerbose],
     "Be verbose, -vv to be even more verbose.")
  , (zipWith ($) [Nullary "-h", Nullary "--help"] (repeat FlagHelp),
     "This usage information.")
  , ([Nullary "--version" FlagVersion],
     "Output version information then exit.") ]

usage = do self <- parameter Config.imageName
           return $ text "Usage:" <+>
             (text self <+> text "[OPTION]..." <+> text "MODULE")

bailout = usage >>= io . printUsage >> io exitFailure

printVersion = do
  self <- parameter Config.imageName
  io $ B.putStrLn $ B.pack $ flip displayS "" $ renderPretty 0.70 100 $
     text "Dedukti" <+> text (showVersion Cabal.version) <> line <> line <>
     text "Copyright (c) 2009 CNRS - École Polytechnique - INRIA." <> line <> line <>
     fillText "You may redistribute copies of Dedukti under the terms of \
              \the GNU General Public License. For more information about \
              \these matters, see the file named COPYING."

initializeConfiguration = foldr aux Config.defaultConfig
    where aux FlagVerbose c     = c { Config.verbosity = Verbose }
          aux FlagVeryVerbose c = c { Config.verbosity = Debug }
          aux (FlagFormat f) c  = c { Config.format = Just f }
          aux (FlagCodeGen g) c = c { Config.cg = Just g }
          aux (FlagJobs n) c    = c { Config.jobs = n }
          aux _ c               = c

main = do
  args <- getArgs
  let (opts, files, errs) = parseCmdline flagDescriptions args
  when (not (null errs)) $ do
         hPutDoc stderr (vcat (map text errs) <> line)
         exitFailure
  (`runDkM` initializeConfiguration opts) $
         case undefined of
           _ | FlagHelp `elem` opts -> do
                     u <- usage
                     io $ printHelp u flagDescriptions
             | FlagVersion `elem` opts -> printVersion
             | FlagMake `elem` opts -> do
                     unless (length files == 1) bailout
                     make [moduleFromPath (head files)]
             | otherwise -> do
                     unless (length files == 1) bailout
                     compile (moduleFromPath (head files))
