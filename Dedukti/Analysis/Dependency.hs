-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Find all direct dependencies of the current module.

module Dedukti.Analysis.Dependency where

import Dedukti.Core
import Dedukti.Module
import qualified Data.Set as Set


collectDependencies :: Module Qid a -> [MName]
collectDependencies drs =
    Set.toList $ Set.fromList [ m | Var x _ <- everyone drs
                                  , Just m <- return (provenance x) ]
