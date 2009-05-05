-- | Global site-specific configuration variables.
module Europa.Config (get, set, Config(..)) where

import Europa.EuM
import Data.IORef
import System.IO.Unsafe


data Config = Config
    { homeDir :: FilePath
    , imageName :: FilePath
    , compiler :: FilePath
    }

-- A mutable cell.
configuration :: IORef Config
configuration = unsafePerformIO $ newIORef undefined

get :: EuM Config
get = liftIO $ readIORef configuration

-- | Retrieve and select configuration information.
select :: (Config -> a) -> EuM a
select sel = get >>= return . sel

-- | Initialize configuration information. This function should never
-- be called beyond initial setup.
set :: Config -> EuM ()
set = liftIO . writeIORef configuration
