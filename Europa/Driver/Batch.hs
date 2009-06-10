-- |
-- Copyright : (c) 2009 INRIA
-- License   : GPL
--
-- The batch driver. It compiles all given targets and all their dependencies,
-- also invoking the Haskell compiler on the generated source code.
module Europa.Driver.Batch (make) where

import Europa.Driver.Compile
import Europa.Analysis.Dependency
import Europa.Module
import Europa.Parser
import Europa.EuM
import qualified Europa.Config as Config
import qualified Control.Hmk.IO as IO
import Control.Hmk
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad
import Control.Applicative
import Data.Typeable (Typeable)
import Control.Exception


cmp x y = do
  s <- io $ IO.isStale x y
  say Debug $ text "Compared" <+> text x <+> text y <> text ":" <+> text (show s)
  return s

-- | Trace a dependency graph in the form of a set of rules, starting from the
-- given root modules. Finding the dependencies of a module requires parsing
-- the corresponding source file. To avoid parsing each file twice, the AST is
-- kept in-memory in case it is needed later during compilation.
rules :: [Module] -> EuM [Rule EuM FilePath]
rules targets = concat <$> mapM f targets where
    -- Collect dependencies.
    f mod = do
      say Verbose $ text "Parsing" <+> text (show mod) <+> text "..."
      let path = srcPathFromModule mod
      src <- parse path <$> io (B.readFile path)
      let dependencies = collectDependencies src
          rs = g mod dependencies (task_compile mod src)
      -- Recursively construct rules for dependent modules.
      rsdeps <- rules dependencies
      say Verbose $ text "Dependencies of" <+> text (show mod) <+> text ":"
              <+> text (show dependencies)
      return $ rs ++ rsdeps
    -- Now that we have the dependencies of the module, we can enounce a few
    -- build rules concerning the module.
    g mod ds compile = let eu  = srcPathFromModule mod
                           euo = objPathFromModule mod
                           hi  = pathFromModule ".hi" mod
                           o   = pathFromModule ".o" mod
                           dephis  = map (pathFromModule ".hi") ds
                       in [ Rule eu [] Nothing cmp
                          , Rule euo [eu] (Just compile) cmp
                          , Rule hi (euo:dephis) (Just $ task_hscomp euo) cmp
                          , Rule o (euo:dephis) (Just $ task_hscomp euo) cmp ]
    task_hscomp euo _ = do
      hscomp <- parameter Config.hsCompiler
      io . IO.testExitCode =<< command hscomp [ "-c", "-x", "hs", euo
                                              , "-XOverloadedStrings" ]
    task_compile mod src _ = do
      -- xxx: should catch any errors here.
      compileAST mod src
      return TaskSuccess

data CommandError = CommandError
    deriving Typeable

instance Show CommandError where
    show CommandError = "Command returned non-zero exit status."

instance Exception CommandError

-- | Perform each system action, aborting if an action returns
-- non-zero exit code.
abortOnError :: [EuM Result] -> EuM ()
abortOnError = mapM_ f where
    f cmd = do code <- cmd
               case code of
                 TaskSuccess -> return ()
                 TaskFailure -> throw CommandError

-- | Compile each of the modules given as input and all of their
-- dependencies, if necessary.
make :: [Module] -> EuM ()
make modules = do
  let targets = map (pathFromModule ".o") modules
  rs <- process cmp <$> rules modules
  schedule <- mk rs targets
  say Debug $ text "Tasks to execute:" <+> int (length schedule)
  abortOnError schedule
