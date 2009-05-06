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
import System.Cmd
import System.Exit


-- | Return a list of all files in the given directory.
getDirectoryFiles :: FilePath -> EuM [FilePath]
getDirectoryFiles dir =
    io $ getDirectoryContents dir >>= filterM (doesFileExist . (dir </>))

cmp x y = io $ IO.isStale x y

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
              cmd_hscomp euo _ = io $ do
                rawSystem hscomp [euo] >>= IO.testExitCode

-- | Compile each of the files given as input and all of their
-- dependencies, if necessary.
make :: [FilePath] -> EuM ()
make targets = do
  files <- getDirectoryFiles "."
  config <- parameter Config.hsCompiler
  sequence_ =<< mk (process cmp (rules config files)) targets
