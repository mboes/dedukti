module Europa.EuM (EuM, runEuM, warn, warnings
                  , text
                  , Exception(..), throw, liftIO) where

import Control.Monad.Trans
import Control.Exception
import Control.Applicative (Applicative)
import Text.PrettyPrint.Leijen


-- | Register a new warning.
warn :: String -> EuM ()
warn = undefined

-- | Get the list of warnings so far.
warnings :: EuM [Doc]
warnings = undefined

newtype EuM a = EuM { runEuM :: IO a }
    deriving (Monad, MonadIO, Functor, Applicative)
