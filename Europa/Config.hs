-- | Global site-specific configuration variables.
module Europa.Config (get, set, Config(..)) where

import System.IORef


data Config = Config
    { homeDir :: FilePath
    , imageName :: FilePath
    }

configuration :: IO (IORef Config)
configuration = newIORef undefined

-- | Retrieve configuration information.
get :: IO Config
get = readIORef config

-- | Initialize configuration information. This function should never
-- be called beyond initial setup.
set :: Config -> IO ()
set = writeIORef config
