-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- The dedukti monad. This provides various facilities such as accumulating
-- warning messages or displaying error messages to the screen. Debugging
-- facilities and an interface to the system are also provided.

module Dedukti.DkM ( module Control.Monad
                   , DkM, runDkM, warn, warnings, say
                   , Verbosity(..)
                   , configuration, parameter
                   , command
                   -- * pretty-printing combinators.
                   , Doc
                   , Pretty(..), text, (<+>), (<>), int
                   , fillText
                   , E.Exception(..), Typeable, E.throw, io
                   -- * Wrappers around IO primitives.
                   , onException) where

import Dedukti.Config as Config
import Control.Monad
import Control.Monad.Reader
import qualified Control.Exception as E
import Control.Applicative
import Data.Typeable (Typeable) -- for exceptions
import System.IO
import System.Cmd
import System.Exit
import Text.PrettyPrint.Leijen hiding ((<$>))


newtype DkM a = DkM (ReaderT Config IO a)
    deriving (Monad, MonadIO, Functor, Applicative, MonadReader Config)

runDkM :: DkM a -> Config -> IO a
runDkM (DkM m) = runReaderT m

-- | Get all global parameters.
configuration :: DkM Config
configuration = ask

-- | Select one parameter.
parameter :: (Config -> a) -> DkM a
parameter sel = sel <$> ask

-- | Wrapper around 'rawSystem'.
command :: String -> [String] -> DkM ExitCode
command exe args = do
  say Verbose $ text "**" <+> text exe <+> hsep (map (squotes . text) args)
  io $ rawSystem exe args

-- | A pretty-printing combinator that outputs filled text with wrapping on
-- word boundaries.
fillText :: String -> Doc
fillText = fillSep . map text . words

-- | Register a new warning.
warn :: String -> DkM ()
warn = undefined

-- | Get the list of warnings so far.
warnings :: DkM [Doc]
warnings = undefined

-- | Write message only if verbosity level is at least the given level.
say :: Verbosity -> Doc -> DkM ()
say v msg = do v' <- parameter Config.verbosity
               when (v <= v') $ io $ hPutDoc stderr (msg <> line)

-- | Shorter name for the oft used 'liftIO'.
io :: IO a -> DkM a
io = liftIO

-- | 'Control.Exception.onException' lifted to the 'DkM' monad.
onException :: DkM a -> DkM b -> DkM a
onException x y = do
  conf <- configuration
  io $ runDkM x conf `E.onException` runDkM y conf
