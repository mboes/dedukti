-- | Global site-specific configuration variables.
module Europa.Config where


data Config = Config
    { homeDir :: FilePath
    , imageName :: FilePath
    , hsCompiler :: FilePath
    }

defaultConfig =
    Config { homeDir = "."
           , imageName = "europa"
           , hsCompiler = "ghc" }

