#!/usr/bin/env runhaskell

> import Distribution.Simple as Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.PackageDescription
> import System.FilePath
> import System.Process
>
> main = defaultMainWithHooks hooks
>   where hooks = simpleUserHooks { Simple.runTests = Main.runTests }
>
> runTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
> runTests _ _ _ lbi = system testprog >> return ()
>   where testprog = (buildDir lbi) </> "dedukti-tests" </> "dedukti-tests"
