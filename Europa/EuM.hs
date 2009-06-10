-- |
-- Copyright : (c) 2009 INRIA
-- License   : GPL
--
-- The europa monad. This provides various facilities such as accumulating
-- warning messages or displaying error messages to the screen. Debugging
-- facilities and an interface to the system are also provided.

module Europa.EuM (EuM, runEuM, warn, warnings, say
                  , Verbosity(..)
                  , configuration, parameter
                  , command
                  , text, (<+>), (<>), int -- pretty-printing combinators.
                  , Exception(..), throw, io) where

import Europa.Config as Config
import Control.Monad.Reader
import Control.Exception
import Control.Applicative
import System.IO
import System.Cmd
import System.Exit
import Text.PrettyPrint.Leijen hiding ((<$>))


instance Applicative (ReaderT Config IO) where
    pure = return
    (<*>) = ap

newtype EuM a = EuM (ReaderT Config IO a)
    deriving (Monad, MonadIO, Functor, Applicative, MonadReader Config)

runEuM :: Config -> EuM a -> IO a
runEuM conf (EuM m) = runReaderT m conf

-- | Get all global parameters.
configuration :: EuM Config
configuration = ask

-- | Select one parameter.
parameter :: (Config -> a) -> EuM a
parameter sel = sel <$> ask

-- | Wrapper around 'rawSystem'.
command :: String -> [String] -> EuM ExitCode
command exe args = do
  say Verbose $ text "**" <+> text exe <+> hsep (map (squotes . text) args)
  io $ rawSystem exe args

-- | Register a new warning.
warn :: String -> EuM ()
warn = undefined

-- | Get the list of warnings so far.
warnings :: EuM [Doc]
warnings = undefined

-- | Write message only if verbosity level is at least the given level.
say :: Verbosity -> Doc -> EuM ()
say v msg = do v' <- parameter Config.verbosity
               when (v <= v') $ io $ hPutDoc stderr (msg <> line)

-- | Shorter name for the oft used 'liftIO'.
io :: IO a -> EuM a
io = liftIO
