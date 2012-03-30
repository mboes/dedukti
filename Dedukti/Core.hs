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
    , bind_name
    , isAbstraction, isVariable, isAtomic, isApplicative
    -- * Environments
    , emptyEnv, env_bindings, env_domain, env_codomain, (&), (!)
    , isin, fromBindings
    -- * Anntoations
    , annot, Unannot, nann, (%%), (%%%), (<%%>), (<%%%>)
    -- * Smart constructors
    , abstract, unabstract, apply, unapply
    -- * Transformations
    , Transform(..), transform, descend
    -- * Query
    , everyone
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Traversable as T
import qualified Data.Map as Map


data Expr id a = B (Binding id a) (Expr id a) a -- ^ Bind an assumption
               | A (Expr id a) (Expr id a) a    -- ^ Application
               | V id a                         -- ^ Variable
               | Type
               | Kind
                 deriving (Eq, Ord, Show)

infix 2 :::

-- | A type decorating a variable, a type on its own, or an expression
-- defining a variable
data Binding id a = L id (Maybe (Expr id a)) -- ^ Lambda binding
                  | id ::: Expr id a         -- ^ Pi binding
                  | id := Expr id a          -- ^ Let binding
                    deriving (Eq, Ord, Show)

-- | A rewrite rule.
data Rule id a = Expr id a :--> Expr id a
                 deriving (Eq, Ord, Show)
infix 9 :-->

-- | An environment is and /ordered/ list of bindings, since types can
-- depend on items defined earlier in environment. We opt for a hybrid
-- representation, for both fast membership tests and conservation of
-- order.
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

type instance Id [t] = Id t
type instance Id (Maybe t) = Id t
type instance Id (Module id a) = id
type instance Id (Binding id a) = id
type instance Id (Rule id a) = id
type instance Id (TyRule id a) = id
type instance Id (RuleSet id a) = id
type instance Id (Expr id a) = id

type instance A  [t] = A t
type instance A  (Maybe t) = A t
type instance A  (Module id a) = a
type instance A  (Binding id a) = a
type instance A  (Rule id a) = a
type instance A  (TyRule id a) = a
type instance A  (RuleSet id a) = a
type instance A  (Expr id a) = a

bind_name :: Binding id a -> id
bind_name (L x _) = x
bind_name (x ::: _) = x
bind_name (x := _) = x

-- | A lambda or Pi abstraction.
isAbstraction :: Expr id a -> Bool
isAbstraction (B (L _ _) _ _) = True
isAbstraction (B (_ ::: _) _ _)  = True
isAbstraction _ = False

isVariable :: Expr id a -> Bool
isVariable (V _ _) = True
isVariable _       = False

isAtomic :: Expr id a -> Bool
isAtomic (V _ _) = True
isAtomic Type = True
isAtomic Kind = True
isAtomic _ = False

-- | An atomic term or an application.
isApplicative :: Expr id a -> Bool
isApplicative (A _ _ _) = True
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

-- | Environment membership.
isin :: Ord id => id -> Env id a -> Bool
isin x (Env _ map) = Map.member x map

fromBindings :: Ord id => [Binding id a] -> Env id a
fromBindings = foldr (&) (Env [] Map.empty)

-- | The type of vacuous annotations.
data Unannot = Unannot deriving (Eq, Ord, Show)

-- | Unannot should stay abstract. 'nann' constructs a value of type 'Unannot'.
nann :: Unannot
nann = Unannot

-- | Annotation extraction.
annot :: Expr id a -> a
annot (B _ _ a) = a
annot (A _ _ a) = a
annot (V _ a)   = a
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

-- | Invariant: in @abstract xs t annots@, @length annots == length xs@.
abstract :: [Binding id a] -> Expr id a -> [a] -> Expr id a
abstract [] t _ = t
abstract (b:bs) t (a:annots) = B b (abstract bs t annots) %% a
abstract bs _ as = error $ "abstract: " ++ show (length bs) ++ " bindings but only "
                                        ++ show (length as) ++ " annotations."

unabstract :: Expr id a -> ([Binding id a] -> Expr id a -> [a] -> r) -> r
unabstract (B b t a) k = unabstract t (\bs t' as -> k (b:bs) t' (a:as))
unabstract t k = k [] t []

-- | Invariant: in @apply t ts annots@, @length annots == length ts@.
apply :: Expr id a -> [Expr id a] -> [a] -> Expr id a
apply t [] _ = t
apply t (x:xs) (a:annots) = apply (A t x %% a) xs annots
apply _ _ _= error "Fewer annotations than number of applications."

-- | Decompose nested applications into a head and a list of arguments.
unapply :: Expr id a -> (Expr id a -> [Expr id a] -> [a] -> r) -> r
unapply t k = go [] [] t where
  go xs as (A t1 t2 a) = go (t2:xs) (a:as) t1
  go xs as t = k t xs as

class Ord (Id t) => Transform t where
    -- | Effectful bottom-up transformation on terms.
    transformM :: Monad m => (Expr (Id t) (A t) -> m (Expr (Id t) (A t))) -> t -> m t
    transformM f = descendM (transformM f)

    -- | Helper function for top-down transformations.
    descendM :: Monad m => (Expr (Id t) (A t) -> m (Expr (Id t) (A t))) -> t -> m t

instance Ord id => Transform (Module id a) where
    descendM f (decls, rules) =
        return (,) `ap` descendM f decls `ap` descendM f rules

instance Ord id => Transform (Binding id a) where
    descendM f (L x (Just ty)) = return (L x) `ap` ((f ty) >>= return . Just)
    descendM f (L x Nothing) = return $ L x Nothing
    descendM f (x ::: ty) = return (x :::) `ap` f ty
    descendM f (x := t) = return (x :=) `ap` f t

instance Ord id => Transform (TyRule id a) where
    descendM f (env :@ rule) =
        return (:@) `ap` (return fromBindings `ap` descendM f (env_bindings env))
                    `ap` descendM f rule

instance Ord id => Transform (Rule id a) where
    descendM f (lhs :--> rhs) = return (:-->) `ap` f lhs `ap` f rhs

instance Ord id => Transform (RuleSet id a) where
    descendM f RS{..} =
        return RS `ap` return rs_name `ap` descendM f rs_type `ap` descendM f rs_rules

instance Ord id => Transform (Expr id a) where
    transformM f = descendM (transformM f) >=> f

    descendM f (B b t a) = return B `ap` descendM f b `ap` f t `ap` return a
    descendM f (A t1 t2 a) = return A `ap` f t1 `ap` f t2 `ap` return a
    descendM f t = return t

instance Transform t => Transform [t] where
    descendM f = T.mapM (descendM f)

instance Transform a => Transform (Maybe a) where
    descendM f = T.mapM (descendM f)

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
