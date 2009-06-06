-- |
-- Copyright : (c) 2009 INRIA
-- License   : GPL

module Europa.Rule where

import Europa.Core
import Data.List (groupBy)
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
arity (_ :@ (lhs :--> _)) = length (unapply lhs) - 1

-- | Combine declarations with their associated rules, if any.
ruleSets :: Eq id => [Binding id a] -> [TyRule id a] -> [RuleSet id a]
ruleSets ds rs = snd $ foldr aux (group rs, []) ds where
    aux (x ::: ty) ([],       rsets)          = ([], RS x ty [] : rsets)
    aux (x ::: ty) (rs : rss, rsets)
        | x == headConstant (Prelude.head rs) = (rss, RS x ty rs : rsets)
        | otherwise                           = (rs : rss, RS x ty [] : rsets)
