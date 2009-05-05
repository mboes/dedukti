module Europa.Driver.Batch (make) where

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
    liftIO $ getDirectoryContents dir >>= filterM (doesFileExist . (dir </>))

cmp x y = liftIO $ IO.isStale x y

-- | Generate a ruleset from the files in the given directory.
rules :: Config.Config -> [FilePath] -> [Rule EuM FilePath]
rules config = concatMap f . filter ((== ".eu") . takeExtension) where
    f file = let stem = dropExtension file
                 euo  = stem <.> ".euo"
                 hi   = stem <.> ".hi"
                 o    = stem <.> ".o"
             in [ Rule file [] Nothing cmp
                , Rule euo  [file] (Just $ cmd_emit file) cmp
                , Rule hi   [euo] (Just $ cmd_compile euo) cmp
                , Rule o    [euo] (Just $ cmd_compile euo) cmp ]
             -- + main.
        where cmd exe args _ = liftIO (rawSystem exe args >>= IO.testExitCode)
              cmd_emit file = cmd (Config.imageName config) [file]
              cmd_compile euo = cmd (Config.compiler config) [euo]

-- | Compile each of the files given as input and all of their
-- dependencies, if necessary.
make :: [FilePath] -> EuM ()
make targets = do
  files <- getDirectoryFiles "t"
  config <- Config.get
  sequence_ =<< mk (process cmp (rules config files)) targets
