-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Global site-specific configuration variables.

module Europa.Config where


data Verbosity = Quiet | Verbose | Debug
                 deriving (Eq, Ord, Show)

data Config = Config
    { homeDir :: FilePath
    , imageName :: FilePath
    , hsCompiler :: FilePath
    , verbosity :: Verbosity
    }

defaultConfig =
    Config { homeDir = "."
           , imageName = "europa"
           , hsCompiler = "ghc"
           , verbosity = Quiet }

