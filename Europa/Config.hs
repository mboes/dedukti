-- | Global site-specific configuration variables.
module Europa.Config where


data Verbosity = Quiet | Verbose | Debug

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

