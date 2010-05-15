-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- The batch driver. It compiles all given targets and all their dependencies
-- (using the Compile driver), also invoking the Haskell compiler on the
-- generated source code.

module Dedukti.Driver.Batch (make) where

import Dedukti.Driver.Compile
import Dedukti.Analysis.Dependency
import Dedukti.Module
import Dedukti.Parser
import Dedukti.DkM
import qualified Dedukti.Config as Config
import qualified Control.Hmk.IO as IO
import Control.Hmk
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative
import System.Directory (copyFile)
import Data.Char (toUpper)


-- The staleness check.
cmp x y = do
  s <- io $ IO.isStale x y
  say Debug $ text "Compared" <+> text x <+> text y <> text ":" <+> text (show s)
  return s

-- | Trace a dependency graph in the form of a set of rules, starting from the
-- given root modules. Finding the dependencies of a module requires parsing
-- the corresponding source file. To avoid parsing each file twice, the AST is
-- kept in-memory in case it is needed later during compilation.
rules :: [MName] -> DkM [Rule DkM FilePath]
rules targets = evalStateT (rules' targets) Map.empty

-- Maintain a list of already seen modules to avoid parsing same modules twice
-- when the dependency graph is not a tree.
rules' targets = concat <$> mapM f targets where
    -- Collect dependencies.
    f mod = do
      seen <- get
      case Map.lookup mod seen of
        Just deps -> return deps
        Nothing -> do
          lift $ say Verbose $ text "Parsing" <+> text (show mod) <+> text "..."
          let path = srcPathFromModule mod
          config <- lift configuration
          src <- lift (parse config path <$> io (B.readFile path))
          let dependencies = collectDependencies src
              rs = g mod dependencies (task_compile mod src)
          -- Recursively construct rules for dependent modules.
          rsdeps <- rules' dependencies
          lift $ say Verbose $ text "Dependencies of" <+> text (show mod) <+> text ":"
                   <+> text (show dependencies)
          put (Map.insert mod (rs ++ rsdeps) seen)
          return $ rs ++ rsdeps
    -- Now that we have the dependencies of the module, we can enounce a few
    -- build rules concerning the module.
    g mod ds compile = let capitalize x = toUpper (head x) : tail x
                           dk  = srcPathFromModule mod
                           dko = objPathFromModule mod
                           dki = ifacePathFromModule mod
                           hi  = pathFromModule ".hi" mod
                           chi = capitalize hi
                           o   = pathFromModule ".o" mod
                           dephis = map (capitalize . pathFromModule ".hi") ds
                       in [ Rule dk [] Nothing cmp
                          , Rule dko [dk] (Just compile) cmp
                          , Rule dki [dk] (Just compile) cmp
                          , Rule hi (dko:dki:dephis) (Just $ task_hscomp dko) cmp
                          , Rule o (dko:dki:dephis) (Just $ task_hscomp dko) cmp
                          , Rule chi [hi] (Just $ task_himv hi chi) cmp ]
    task_hscomp dko _ = abortOnError $ do
      hscomp <- parameter Config.hsCompiler
      io . IO.testExitCode =<< command hscomp [ "-c", "-w", "-x", "hs", dko
                                              , "-XOverloadedStrings"
                                              , "-XPatternGuards" ]
    -- GHC won't find the interface files if their names don't start with a
    -- capital letter. So alias the interface file with a capitalized name.
    task_himv hi chi _ = abortOnError $ do
      io $ copyFile hi chi
      return TaskSuccess
    task_compile mod src _ = abortOnError $ do
      compileAST mod src `onException`
          (say Quiet $ text "In module" <+> pretty mod <> text ":")
      return TaskSuccess

data CommandError = CommandError
                    deriving Typeable

instance Show CommandError where
    show CommandError = "Command returned non-zero exit status."

instance Exception CommandError

-- | Perform each system action, aborting if an action returns
-- non-zero exit code.
abortOnError :: DkM Result -> DkM Result
abortOnError cmd = do
  code <- cmd
  case code of
    TaskFailure -> throw CommandError
    x -> return x

-- | Compile each of the modules given as input and all of their
-- dependencies, if necessary.
make :: [MName] -> DkM ()
make modules = do
  config <- configuration
  let targets = map (pathFromModule ".o") modules
      run :: Rule DkM a -> Rule IO a
      run Rule{..} = Rule{ recipe = fmap (\f x -> runDkM (f x) config) recipe
                         , isStale = \x y -> runDkM (isStale x y) config
                         , .. }
  rs <- process cmp <$> rules modules
  -- Depending on whether we have several cores or not, we either call
  -- mkConcurrent to perform all the tasks concurrently, or call mk to create
  -- a schedule and execute that.
  n <- parameter Config.jobs
  if n > 1 then
      do io $ mkConcurrent n (map run rs) targets else
      do schedule <- mk rs targets
         say Verbose $ text "Tasks to execute:" <+> int (length schedule)
         sequence_ schedule
