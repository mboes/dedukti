#!/usr/bin/env runhaskell

> import Distribution.Simple as Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.PackageDescription
> import System.FilePath
> import System.Process
>
> main = defaultMainWithHooks simpleUserHooks
