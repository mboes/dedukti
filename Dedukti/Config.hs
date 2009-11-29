-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Global site-specific configuration variables.

module Dedukti.Config where

import System.Environment (getProgName)
import System.IO.Unsafe (unsafePerformIO)


data Verbosity = Quiet | Verbose | Debug
                 deriving (Eq, Ord, Show)

data Config = Config
    { imageName :: FilePath
    , version :: String
    , hsCompiler :: FilePath
    , verbosity :: Verbosity
    }

defaultConfig =
    Config { imageName = unsafePerformIO $ getProgName
           , hsCompiler = "ghc"
           , version = "0.1"
           , verbosity = Quiet }

