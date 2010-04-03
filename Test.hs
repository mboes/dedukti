module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.API
import System.FilePath
import System.Directory
import System.Process
import System.Exit
import Control.Exception
import Data.Typeable
import Prelude hiding (catch)

data CheckResult = CheckOk
                   -- ^ The property is true as far as we could check it
                 | CheckFailed String
                   -- ^ The property was not true. The string is the reason.
                 | CheckNoExpectedFailure
                   -- ^ We expected that a property would fail but it didn't
                 | CheckTimedOut
                   -- ^ The property timed out during execution
                 | CheckException String
                   -- ^ The property raised an exception during execution
                   deriving Typeable

instance Show CheckResult where
  show CheckOk = "Ok, passed."
  show (CheckFailed err) = "Failed: " ++ err
  show CheckNoExpectedFailure = "No expected failure."
  show CheckTimedOut = "Timed out."
  show (CheckException err) = "Exception: " ++ err

instance Exception CheckResult

data CheckRunning = CheckRunning
                    deriving Show

instance TestResultlike CheckRunning CheckResult where
    testSucceeded CheckOk = True
    testSucceeded _ = False

data Expectation = ExpectSuccess | ExpectFailure

data Check = Check { chk_test :: String
                   , chk_deps :: [String]
                   , chk_expect :: Expectation }

instance Testlike CheckRunning CheckResult Check where
    runTest topts Check{..} = runImprovingIO $ do
      yieldImprovement CheckRunning
      result <- maybeTimeoutImprovingIO (unK $ topt_timeout topts) $
                liftIO (dkrun chk_test chk_deps chk_expect)
      return (result `orElse` CheckTimedOut)

    testTypeName _ = "Type Checking Runs"

dkrun :: FilePath -> [FilePath] -> Expectation -> IO CheckResult
dkrun dk deps exp = do
  cwd <- getCurrentDirectory
  setCurrentDirectory $ cwd </> "t"
  result <- catch (do
    let compile = rawSystem "../dist/build/dedukti/dedukti" [dk <.> "dk"]
        run = rawSystem "../scripts/dkrun" $ (dk <.> "dko") : map (<.> "dko") deps
    test (return ()) fail compile
    case exp of
      ExpectSuccess -> test (return CheckOk) fail run
      ExpectFailure -> test (return CheckNoExpectedFailure) (const $ return CheckOk) run)
            (\e -> return (e :: CheckResult))
  setCurrentDirectory cwd
  return result
    where test s f m = m >>= \code -> case code of
            ExitSuccess -> s
            ExitFailure x -> f x
          fail x = throw $ CheckFailed $ "exit status " ++ show x

check :: TestName -> [String] -> Test
check name deps = Test name (Check name deps ExpectSuccess)

checkFailure :: TestName -> [String] -> Test
checkFailure name deps = Test name (Check name deps ExpectFailure)

-- * Actual tests.

main = defaultMain tests

tests = [ testGroup "Smoke tests"
          [ check "nat" []
          , check "coc" []
          , check "logic" ["coc"]
          , check "peano" ["logic", "coc"]
          , checkFailure "bug" [] ] ]
