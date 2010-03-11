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
import Data.List (partition, isPrefixOf)
import Data.Either (partitionEithers)
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Paths_dedukti as Cabal (version)
import Data.Version


data Flag = FlagMake
          | FlagFormat Config.Format
          | FlagHelp | FlagVersion
          | FlagVerbose | FlagVeryVerbose
            deriving (Eq, Ord, Show)

flagDescriptions =
  [ (["--make"], [FlagMake],
     "Build MODULE and all its dependencies in one go.")
  , (["-fexternal"], [FlagFormat Config.External],
     "Force recognizing input as (human-readable) external format.")
  , (["-fprefix-notation"], [FlagFormat Config.Prefix],
     "Force recognizing input as (fast) prefix format.")
  , (["-v", "-vv"], [FlagVerbose, FlagVeryVerbose],
     "Be verbose, -vv to be even more verbose.")
  , (["-h", "--help"], repeat FlagHelp,
     "This usage information.")
  , (["--version"], [FlagVersion],
     "Output version information then exit.") ]

parseCmdline args =
  let (hyphened, rest) = partition (\arg -> "-" `isPrefixOf` arg) args
      (errors, flags) = partitionEithers $ map toFlag hyphened
  in (flags, rest, errors)
    where flagmap = concat $ map (uncurry zip) $ map (\(x, y, _) -> (x, y)) flagDescriptions
          toFlag x = case lookup x flagmap of
            Nothing -> Left "Flag not found."
            Just f -> Right f

printUsage = do
  self <- parameter Config.imageName
  let header = show $ text "Usage:" <+>
               (text self <+> text "[OPTION]..." <+> text "MODULE")
  io $ hPutStrLn stderr header

printHelp = do
  self <- parameter Config.imageName
  let header = text "Usage:" <+>
               (text self <+> text "[OPTION]..." <+> text "MODULE")
               <$> text "Options:"
      flags = vsep (map pflag flagDescriptions)
  io $ putStrLn $ show $ header <$> indent 4 flags
    where pflag (flags, _, desc) = fillBreak 14 (hcat $ punctuate (text ", ") $
                                   map text flags) <+> text desc

bailout = printUsage >> io exitFailure

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
          aux _ c               = c

main = do
  args <- getArgs
  let (opts, files, errs) = parseCmdline args
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
