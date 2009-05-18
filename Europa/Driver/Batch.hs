module Europa.Driver.Batch (make) where

import Europa.Driver.Compile
import Europa.Module
import Europa.EuM
import qualified Europa.Config as Config
import Control.Hmk
import qualified Control.Hmk.IO as IO
import Control.Monad
import System.FilePath
import System.Directory
import Data.Typeable (Typeable)
import Control.Exception


-- | Return a list of all files in the given directory.
getDirectoryFiles :: FilePath -> EuM [FilePath]
getDirectoryFiles dir =
    io $ getDirectoryContents dir >>= filterM (doesFileExist . (dir </>))

cmp x y = do
  s <- io $ IO.isStale x y
  say Debug $ text "Compared" <+> text x <+> text y <> text ":" <+> text (show s)
  return s

-- | Generate a ruleset from the files in the given directory.
rules :: String -> [FilePath] -> [Rule EuM FilePath]
rules hscomp = concatMap f . filter ((== ".eu") . takeExtension) where
    f file = let stem = dropExtension file
                 euo  = stem <.> ".euo"
                 hi   = stem <.> ".hi"
                 o    = stem <.> ".o"
             in [ Rule file [] Nothing cmp
                , Rule euo  [file] (Just $ cmd_compile file) cmp
                , Rule hi   [euo] (Just $ cmd_hscomp euo) cmp
                , Rule o    [euo] (Just $ cmd_hscomp euo) cmp ]
             -- + main.
        where cmd_compile file _ = do
                compile (moduleFromPath file)
                return TaskSuccess
              cmd_hscomp euo _ =
                  command hscomp [ "-x", "hs", euo
                                 , "-XOverloadedStrings" ]
                  >>= io . IO.testExitCode

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
  files <- getDirectoryFiles "."
  config <- parameter Config.hsCompiler
  schedule <- mk (process cmp (rules config files)) targets
  say Debug $ text "Tasks to execute:" <+> int (length schedule)
  abortOnError schedule
