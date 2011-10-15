-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL

module Dedukti.Core
    ( -- * Terms
      Expr(..), Binding(..)
    -- * Rules
    , Rule(..), Env, TyRule(..), RuleSet(..)
    -- * Type Synonyms
    , Module
    -- * Type functions
    , Id, A
    -- * Convenience functions
    , (.->)
    , bind_name, bind_type
    , isAbstraction, isVariable, isAtomic, isApplicative
    -- * Environments
    , emptyEnv, env_bindings, env_domain, env_codomain, (&), (!)
    , isin, fromBindings
    -- * Anntoations
    , annot, Unannot, nann, (%%), (%%%), (<%%>), (<%%%>)
    -- * Smart constructors
    , abstract, apply, unapply
    -- * Transformations
    , Transform(..), transform, descend
    -- * Query
    , everyone
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as Map


data Expr id a = Lam (Binding id a) (Expr id a) a
               | Pi  (Binding id a) (Expr id a) a
               | Let (Binding id a) (Expr id a) a
               | App (Expr id a) (Expr id a) a
               | Var id a
               | Type
               | Kind
                 deriving (Eq, Ord, Show)

infix 2 :::

-- | A type decorating a variable, a type on its own, or an expression defining a variable
data Binding id a = id ::: Expr id a
                  | Hole (Expr id a)
                  | id := Expr id a
                    deriving (Eq, Ord, Show)

-- | A rewrite rule.
data Rule id a = Expr id a :--> Expr id a
                 deriving (Eq, Ord, Show)
infix 9 :-->

-- | An environment is and *ordered* list of bindings, since types can depend
-- on items defined earlier in environment. We opt for a hybrid
-- representation, for both fast membership tests and conservation of order.
data Env id a = Env [Binding id a] (Map.Map id (Expr id a))
                deriving (Eq, Ord, Show)

-- | A rewrite rule paired with a typing environment.
data TyRule id a = Env id a :@ Rule id a
                 deriving (Eq, Ord, Show)
infix 8 :@

-- | A set of rewrite rules sharing a same head constant.
-- Invariant:
--
-- > all ((== rs_name ruleset) . headConstant) (rs_rules ruleset)
data RuleSet id a = RS { rs_name :: id
                       , rs_type :: Expr id a
                       , rs_rules :: [TyRule id a] }
                    deriving (Eq, Ord, Show)

type Module id a = ([Binding id a], [TyRule id a])

-- | Type families used to project out the type parameters from the datatypes.
type family Id t
type family A t

type instance Id (Module id a) = id
type instance Id (Binding id a) = id
type instance Id (Rule id a) = id
type instance Id (TyRule id a) = id
type instance Id (RuleSet id a) = id
type instance Id (Expr id a) = id

type instance A  (Module id a) = a
type instance A  (Binding id a) = a
type instance A  (Rule id a) = a
type instance A  (TyRule id a) = a
type instance A  (RuleSet id a) = a
type instance A  (Expr id a) = a

(.->) :: Expr id a -> Expr id a -> a -> Expr id a
x .-> y = Pi (Hole x) y
infixr .->

bind_type :: Binding id a -> Expr id a
bind_type (_ ::: ty) = ty
bind_type (Hole ty) = ty
bind_type (_ := _) = error "Binding has no type."

bind_name :: Binding id a -> id
bind_name (x ::: _) = x
bind_name (Hole _) = error "Binding has no name."
bind_name (x := _) = x

binding :: MonadPlus m => Expr id a -> m (Binding id a)
binding (Lam b t _) = return b
binding (Pi b t _) = return b
binding (Let b t _) = return b
binding _ = mzero

isAbstraction :: Expr id a -> Bool
isAbstraction (Lam _ _ _) = True
isAbstraction (Pi _ _ _)  = True
isAbstraction _           = False

isVariable :: Expr id a -> Bool
isVariable (Var _ _) = True
isVariable _         = False

isAtomic :: Expr id a -> Bool
isAtomic (Var _ _) = True
isAtomic Type      = True
isAtomic Kind      = True
isAtomic _         = False

isApplicative :: Expr id a -> Bool
isApplicative (App _ _ _) = True
isApplicative t = isAtomic t

env_bindings (Env bs _) = bs
env_domain (Env bs map) = Map.keys map
env_codomain (Env bs map) = Map.elems map

infix 1 &

emptyEnv = Env [] Map.empty

-- | Extend an environment with a new binding.
(&) :: Ord id => Binding id a -> Env id a -> Env id a
x ::: ty & Env bs map = Env ((x ::: ty) : bs) (Map.insert x ty map)
_ & Env _ _ = error "Binding is not a typing assumption."

(!) :: Ord id => Env id a -> id -> Expr id a
Env _ map ! x = map Map.! x

isin :: Ord id => id -> Env id a -> Bool
isin x (Env _ map) = Map.member x map

fromBindings :: Ord id => [Binding id a] -> Env id a
fromBindings = foldr (&) (Env [] Map.empty)

-- | Phantom type used to express no annotation.
data Unannot = Unannot deriving (Eq, Ord, Show)

-- | Unannot should stay abstract. |nann| constructs a value of type |Unannot|.
nann = Unannot

-- | Annotation extraction.
annot (Lam _ _ a) = a
annot (Pi _ _ a)  = a
annot (Let _ _ a) = a
annot (App _ _ a) = a
annot (Var _ a)   = a
annot _ = error "No annotation."


-- | Annotation operator.
(%%) :: (a -> Expr id a) -> a -> Expr id a
(%%) = ($)

-- | Apply annotations to an annotation expecting context.
(%%%) :: ([a] -> Expr id a) -> [a] -> Expr id a
(%%%) = ($)

-- | Applicative annotation operator.
(<%%>) :: Applicative f => f (a -> Expr id a) -> a -> f (Expr id a)
x <%%> a = x <*> pure a

-- | Applicative multi-annotation operator.
(<%%%>) :: Applicative f => f ([a] -> Expr id a) -> [a] -> f (Expr id a)
x <%%%> a = x <*> pure a

infixl 1 %%
infixl 1 %%%
infixl 1 <%%>
infixl 1 <%%%>

-- | Invariant: in abstract xs t annots, length annots == length xs.
abstract :: [Binding id a] -> Expr id a -> [a] -> Expr id a
abstract [] t _ = t
abstract (x:xs) t (a:annots) = Lam x (abstract xs t annots) %% a
abstract _ _ _ = error "Fewer annotations than number of variables."

unabstract :: Expr id a -> ([Binding id a] -> Expr id a -> [a] -> r) -> r
unabstract (Lam b t a) k = unabstract t (\bs t' as -> k (b:bs) t' (a:as))
unabstract t k = k [] t []

-- | Invariant: in apply ts annots, length annots == length ts - 1.
apply :: Expr id a -> [Expr id a] -> [a] -> Expr id a
apply t [] _ = t
apply t (x:xs) (a:annots) = apply (App t x %% a) xs annots
apply _ _ _= error "Fewer annotations than number of applications."

-- | Turn nested applications into a list.
unapply :: Expr id a -> (Expr id a -> [Expr id a] -> [a] -> r) -> r
unapply t k = go [] [] t where
  go xs as (App t1 t2 a) = go (t2:xs) (a:as) t1
  go xs as t = k t xs as


class Ord (Id t) => Transform t where
    -- | Effectful bottom-up transformation on terms.
    transformM :: Monad m => (Expr (Id t) (A t) -> m (Expr (Id t) (A t))) -> t -> m t

    -- | Helper function for top-down transformations.
    descendM :: Monad m => (Expr (Id t) (A t) -> m (Expr (Id t) (A t))) -> t -> m t

instance Ord id => Transform (Module id a) where
    transformM f (decls, rules) =
        return (,) `ap` mapM (transformM f) decls `ap` mapM (transformM f) rules

    descendM f (decls, rules) =
        return (,) `ap` mapM (descendM f) decls `ap` mapM (descendM f) rules

instance Ord id => Transform (Binding id a) where
    transformM f (x ::: ty) = return (x :::) `ap` transformM f ty
    transformM f (Hole ty) = return Hole `ap` transformM f ty
    transformM f (x := t) = return (x :=) `ap` transformM f t

    descendM f (x ::: ty) = return (x :::) `ap` f ty
    descendM f (Hole ty) = return Hole `ap` f ty
    descendM f (x := t) = return (x :=) `ap` f t

instance Ord id => Transform (TyRule id a) where
    transformM f (env :@ rule) =
        return (:@) `ap` (return fromBindings `ap` mapM (transformM f) (env_bindings env)) `ap`
               transformM f rule

    descendM f (env :@ rule) =
        return (:@) `ap` (return fromBindings `ap` mapM (descendM f) (env_bindings env)) `ap`
               descendM f rule

instance Ord id => Transform (Rule id a) where
    transformM f (lhs :--> rhs) =
        return (:-->) `ap` transformM f lhs `ap` transformM f rhs
    descendM f (lhs :--> rhs) = return (:-->) `ap` f lhs `ap` f rhs

instance Ord id => Transform (RuleSet id a) where
    transformM f RS{..} =
        return RS `ap` return rs_name `ap` transformM f rs_type `ap` mapM (transformM f) rs_rules

    descendM f RS{..} =
        return RS `ap` return rs_name `ap` descendM f rs_type `ap` mapM (descendM f) rs_rules

instance Ord id => Transform (Expr id a) where
    transformM f (Lam (x ::: ty) t a) = do
      ty' <- transformM f ty
      t' <- transformM f t
      f $ Lam (x ::: ty') t' a
    transformM f (Lam (Hole ty) t a) = do
      ty' <- transformM f ty
      t' <- transformM f t
      f $ Lam (Hole ty') t' a
    transformM f (Pi (x ::: ty) t a) = do
      ty' <- transformM f ty
      t' <- transformM f t
      f $ Pi (x ::: ty') t' a
    transformM f (Pi (Hole ty) t a) = do
      ty' <- transformM f ty
      t' <- transformM f t
      f $ Pi (Hole ty') t' a
    transformM f (App t1 t2 a) = do
      f =<< return App `ap` transformM f t1 `ap` transformM f t2 `ap` return a
    transformM f t = f t

    descendM f (Lam (x ::: ty) t a) = do
      ty' <- f ty
      t' <- f t
      return $ Lam (x ::: ty') t' a
    descendM f (Lam (Hole ty) t a) = do
      ty' <- f ty
      t' <-  f t
      return $ Lam (Hole ty') t' a
    descendM f (Pi (x ::: ty) t a) = do
      ty' <- f ty
      t' <- f t
      return $ Pi (x ::: ty') t' a
    descendM f (Pi (Hole ty) t a) = do
      ty' <- f ty
      t' <- f t
      return $ Pi (Hole ty') t' a
    descendM f (App t1 t2 a) = do
      return App `ap` f t1 `ap` f t2 `ap` return a
    descendM f t = return t

-- | Pure bottom-up transformation on terms.
transform :: Transform t => (Expr (Id t) (A t) -> Expr (Id t) (A t)) -> t -> t
transform f = runIdentity . transformM (return . f)

descend :: Transform t => (Expr (Id t) (A t) -> Expr (Id t) (A t)) -> t -> t
descend f = runIdentity . descendM (return . f)

-- | Produces all substructures of the given term. Often useful as a generator
-- in a list comprehension.
everyone :: Transform t => t -> [Expr (Id t) (A t)]
everyone t = execState (transformM f t) [] where
    f t = withState (t:) (return t)
