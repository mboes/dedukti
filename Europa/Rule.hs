-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Various utility functions over rewrite rules and rule sets.

module Europa.Rule where

import Europa.Core
import Data.List (groupBy, sortBy)
import qualified Data.Stream as Stream
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude hiding (head)
import qualified Prelude


head :: TyRule id a -> Expr id a
head (_ :@ (lhs :--> _)) = lhs

headConstant :: TyRule id a -> id
headConstant = unvar . Prelude.head . unapply . head where
    unvar (Var x _) = x

patterns :: TyRule id a -> [Expr id a]
patterns = Prelude.tail . unapply . head

-- | Group set of rules by head constant.
group :: Eq id => [TyRule id a] -> [[TyRule id a]]
group = groupBy f where
    f x y = headConstant x == headConstant y

arity :: TyRule id a -> Int
arity (_ :@ lhs :--> _) = length (unapply lhs) - 1

-- | Combine declarations with their associated rules, if any.
ruleSets :: (Show id, Show a, Ord id) => [Binding id a] -> [TyRule id a] -> [RuleSet id a]
ruleSets ds rs = snd $ foldl aux (sortBy cmp (group rs), []) ds where
    aux ([],       rsets) (x ::: ty)          = ([], RS x ty [] : rsets)
    aux (rs : rss, rsets) (x ::: ty)
        | x == headConstant (Prelude.head rs) = (rss, RS x ty rs : rsets)
        | otherwise                           = (rs : rss, RS x ty [] : rsets)
    -- We cannot change the order of the declarations, but we need rules to be
    -- in the same order as the declarations.
    ordering = Map.fromList (zip (map bind_name ds) [0..])
    cmp x y = let xi = ordering Map.! headConstant (Prelude.head x)
                  yi = ordering Map.! headConstant (Prelude.head y)
              in compare xi yi

-- | Make the rule left-linear and return collected unification constraints.
-- This function must be provided with an infinite supply of fresh variable
-- names.
linearize :: Ord id => Stream.Stream id -> TyRule id a -> (TyRule id a, [(id, id)])
linearize xs (env :@ lhs :--> rhs) =
              let (lhs', (_, _, constraints)) = runState (transformM f lhs) (xs, Set.empty, [])
                  -- Add new variables to environment, with same type as of
                  -- the variables they are unified to.
                  env' = foldr (\(x,x') env -> x' ::: (env ! x) & env) env constraints
              in (env' :@ lhs' :--> rhs, constraints)
    where f t@(Var x a) = do
            (xs, seen, constraints) <- get
            if x `Set.member` seen then
                do let Stream.Cons x' xs' = xs
                   put (xs', Set.insert x seen, (x, x'):constraints)
                   return $ Var x' a else
                do put (xs, Set.insert x seen, constraints)
                   return t
          f t = return t
