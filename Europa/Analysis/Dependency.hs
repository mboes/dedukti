-- |
-- Copyright : (c) 2009 INRIA
-- License   : GPL
--
-- Find all direct dependencies of the current module.

module Europa.Analysis.Dependency where

import Europa.Core
import Europa.Module
import qualified Data.Set as Set


collectDependencies :: Module Qid a -> [MName]
collectDependencies drs =
    Set.toList $ Set.fromList [ m | Var x _ <- everyone drs
                                  , Just m <- return (provenance x) ]
