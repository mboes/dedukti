-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Various utility functions over rewrite rules and rule sets.

module Dedukti.Rule where

import Dedukti.Core
import qualified Dedukti.Reduction as Red
import Data.List (groupBy, sortBy)
import qualified Data.Stream as Stream
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude hiding (head)
import qualified Prelude


-- | Left hand side of a rule.
head :: TyRule id a -> Expr id a
head (_ :@ lhs :--> _) = lhs

-- | The head of the head of the rule.
headConstant :: TyRule id a -> id
headConstant r = unabstract (head r) $ \_ t _ -> unapply t $ \(V x _) _ _ -> x

-- | The patterns to which the head constant is applied.
patterns :: Ord id => TyRule id a -> [Expr id a]
patterns r = unapply (Red.zeta (head r)) $ \_ ts _ -> ts

-- | Group set of rules by head constant.
group :: Eq id => [TyRule id a] -> [[TyRule id a]]
group = groupBy f where
    f x y = headConstant x == headConstant y

arity :: Ord id => TyRule id a -> Int
arity = length . patterns

-- | Combine declarations with their associated rules, if any.
ruleSets :: (Show id, Show a, Ord id) => [Binding id a] -> [TyRule id a] -> [RuleSet id a]
ruleSets ds rs = snd $ foldr aux (sortBy cmp (group rs), []) ds where
    aux (L x ::: ty) ([],       rsets)          = ([], RS x ty [] : rsets)
    aux (L x ::: ty) (rs : rss, rsets)
        | x == headConstant (Prelude.head rs) = (rss, RS x ty rs : rsets)
        | otherwise                           = (rs : rss, RS x ty [] : rsets)
    -- We cannot change the order of the declarations, but we need rules to be
    -- in the same order as the declarations.
    ordering = Map.fromList (zip (map bind_name ds) [0..])
    cmp x y = let xi = ordering Map.! headConstant (Prelude.head x)
                  yi = ordering Map.! headConstant (Prelude.head y)
              in compare yi xi

-- | Make the rule left-linear and return collected unification constraints.
-- This function must be provided with an infinite supply of fresh variable
-- names.
linearize :: Ord id => Stream.Stream id -> TyRule id a -> (TyRule id a, [(id, id)])
linearize xs (env :@ lhs :--> rhs) =
    let (lhs', (_, _, constraints)) = runState (transformM f lhs) (xs, Set.empty, [])
        -- Add new variables to the environment, with same type as
        -- that of the variables they are unified to.
        env' = foldr (\(x,x') env -> L x' ::: (env ! x) & env) env constraints
    in (env' :@ lhs' :--> rhs, constraints)
    where f t@(V x a) | x `isin` env = do
            (xs, seen, constraints) <- get
            if x `Set.member` seen then
                do let Stream.Cons x' xs' = xs
                   put (xs', Set.insert x' seen, (x, x'):constraints)
                   return $ V x' a else
                do put (xs, Set.insert x seen, constraints)
                   return t
          f t = return t
