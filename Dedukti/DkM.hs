-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- The europa monad. This provides various facilities such as accumulating
-- warning messages or displaying error messages to the screen. Debugging
-- facilities and an interface to the system are also provided.

module Dedukti.DkM ( module Control.Monad
                  , DkM, runDkM, warn, warnings, say
                  , Verbosity(..)
                  , configuration, parameter
                  , command
                  -- pretty-printing combinators.
                  , Pretty(..), text, (<+>), (<>), int
                  , fillText
                  , E.Exception(..), Typeable, E.throw, io
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


instance Applicative (ReaderT Config IO) where
    pure = return
    (<*>) = ap

newtype DkM a = DkM (ReaderT Config IO a)
    deriving (Monad, MonadIO, Functor, Applicative, MonadReader Config)

runDkM :: Config -> DkM a -> IO a
runDkM conf (DkM m) = runReaderT m conf

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

onException :: DkM a -> DkM b -> DkM a
onException x y = do
  conf <- configuration
  io $ runDkM conf x `E.onException` runDkM conf y
