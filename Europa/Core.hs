-- |
-- Copyright : (c) 2009 INRIA
-- License   : GPL

module Europa.Core
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
    , range, isAbstraction, isApplication, isVariable, isAtomic, isApplicative
    -- * Environments
    , (&)
    -- * Annotations
    , Unannot, nann, (%%), (%%%), (<%%>), (<%%%>)
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
               | App (Expr id a) (Expr id a) a
               | Var id a
               | Type
               | Kind
                 deriving (Eq, Ord, Show)

infix 2 :::

-- | A type decorating a variable, or a type on its own.
data Binding id a = id ::: Expr id a
               | Hole (Expr id a)
                 deriving (Eq, Ord, Show)

-- | A rewrite rule.
data Rule id a = Expr id a :--> Expr id a
                 deriving (Eq, Ord, Show)
infix 9 :-->

type Env id a = Map.Map id (Expr id a)

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

type Module id a = ([Binding id a], [TyRule id a])

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

x .-> y = Pi (Hole x) y
infixr .->

range (Hole ty) = ty
range (x ::: ty) = ty

isAbstraction (Lam _ _ _) = True
isAbstraction (Pi _ _ _)  = True
isAbstraction _           = False

isApplication (App _ _ _) = True
isApplication _           = False

isVariable (Var _ _) = True
isVariable _         = False

isAtomic (Var _ _) = True
isAtomic Type      = True
isAtomic Kind      = True
isAtomic _         = False

isApplicative x = isAtomic x || isApplication x

infix 1 &

-- | Extend an environment with a new binding.
(&) :: Ord id => Env id a -> Binding id a -> Env id a
env & x ::: t = Map.insert x t env

-- | Phantom type used to express no annotation.
data Unannot = Unannot deriving (Eq, Ord, Show)

-- | Unannot should stay abstract. |nann| constructs a value of type |Unannot|.
nann = Unannot

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

-- | Invariant: in apply ts annots, length annots == length ts - 1.
apply :: Expr id a -> [Expr id a] -> [a] -> Expr id a
apply t [] _ = t
apply t (x:xs) (a:annots) = apply (App t x %% a) xs annots
apply _ _ _= error "Fewer annotations than number of applications."

-- | Turn nested applications into a list.
unapply :: Expr id a -> [Expr id a]
unapply = reverse . aux where
    aux (App t1 t2 _) = t2 : aux t1
    aux t = [t]

class Ord (Id t) => Transform t where
    -- | Effectful bottom-up transformation on terms.
    transformM :: (Monad m, Ord (Id t)) => (Expr (Id t) (A t) -> m (Expr (Id t) (A t))) -> t -> m t

    -- | Helper function for top-down transformations.
    descendM :: (Monad m, Ord (Id t)) => (Expr (Id t) (A t) -> m (Expr (Id t) (A t))) -> t -> m t

instance Ord id => Transform (Module id a) where
    transformM f (decls, rules) =
        return (,) `ap` mapM (transformM f) decls `ap` mapM (transformM f) rules

    descendM f (decls, rules) =
        return (,) `ap` mapM (descendM f) decls `ap` mapM (descendM f) rules

instance Ord id => Transform (Binding id a) where
    transformM f (x ::: ty) = return (x :::) `ap` transformM f ty
    transformM f (Hole ty) = return Hole `ap` transformM f ty

    descendM f (x ::: ty) = return (x :::) `ap` f ty
    descendM f (Hole ty) = return Hole `ap` f ty

instance Ord id => Transform (TyRule id a) where
    transformM f (env :@ rule) =
        return (:@) `ap`
                   (return Map.fromList `ap`
                           mapM (\(x, ty) -> transformM f ty >>= return . ((,) x))
                                    (Map.toList env)) `ap`
                   transformM f rule

    descendM f (env :@ rule) =
        return (:@) `ap`
                   (return Map.fromList `ap`
                           mapM (\(x, ty) -> f ty >>= return . ((,) x))
                                    (Map.toList env)) `ap`
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
