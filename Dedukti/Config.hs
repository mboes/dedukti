-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Global site-specific configuration variables.

module Dedukti.Config where


data Verbosity = Quiet | Verbose | Debug
                 deriving (Eq, Ord, Show)

data Config = Config
    { imageName :: FilePath
    , version :: String
    , hsCompiler :: FilePath
    , verbosity :: Verbosity
    }

defaultConfig =
    Config { imageName = "dedukti"
           , hsCompiler = "ghc"
           , version = "0.1"
           , verbosity = Quiet }

