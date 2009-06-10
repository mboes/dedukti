module Europa.Analysis.Dependency where

import Europa.Core
import Europa.Module
import qualified Data.Set as Set
import qualified Data.Map as Map


collectDependencies :: ([Binding Qid a], [TyRule Qid a]) -> [Module]
collectDependencies (decls, rules) = Set.toList (ddeps `Set.union` rdeps)
    where ddeps = Set.unions (map (extract .  range) decls) where
          rdeps = Set.unions (map f rules) where
              f (env :@ lhs :--> rhs) =
                  Set.unions (map extract (Map.elems env))
                    `Set.union` extract lhs
                    `Set.union` extract rhs
          extract t = Set.fromList [ m | Var x _ <- everyone t, Just m <- [provenance x] ]
