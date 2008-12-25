module Europa.Core where

import Control.Applicative as A
import Data.Traversable as A
import Data.Foldable as A


data Expr id a = Lam (TVar id a) (Expr id a) a
               | Pi  (TVar id a) (Expr id a) a
               | App (Expr id a) (Expr id a) a
               | Var id a
               | Type
               | Kind
                 deriving (Eq, Ord, Show)

data TVar id a = id ::: Expr id a
               | Hole (Expr id a)
                 deriving (Eq, Ord, Show)

data Rule id a = Expr id a :--> Expr id a
                 deriving (Eq, Ord, Show)
infix 9 :-->

data TyRule id a = [TVar id a] :@ Rule id a
                   deriving Show
infix 8 :@

x .-> y = Pi (Hole x) y
infixr .->

range (Pi (Hole ty) _ _) = ty
range (Pi (x ::: ty) _ _) = ty
range (Lam (Hole ty) _ _) = ty
range (Lam (x ::: ty) _ _) = ty
range _ = error "'range' only applicable to arrow types."

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

-- Phantom type used to express no annotation.
data Unannot
nann = error "No annotation."

instance Show Unannot where
    show _ = "*"

-- | Invariant: in abstract xs t annots, length annots == length xs.
abstract :: [TVar id a] -> Expr id a -> [a] -> Expr id a
abstract [] t _ = t
abstract (x:xs) t (a:annots) = Lam x (abstract xs t annots) %% a
abstract _ _ _ = error "Fewer annotations than number of variables."

-- | Invariant: in apply ts annots, length annots == length ts - 1.
apply :: Expr id a -> [Expr id a] -> [a] -> Expr id a
apply t [] _ = t
apply t (x:xs) (a:annots) = apply (App t x %% a) xs annots
apply _ _ _= error "Fewer annotations than number of applications."

rot3 :: (b -> c -> a -> d) -> a -> b -> c -> d
rot3 f x y z = f y z x

instance Traversable (Expr id) where
    -- We want a preorder traversal of Exp here, hence the need for rot3.
    traverse f (Lam x t ann) = rot3 Lam <$> f ann <*> traverse f x <*> traverse f t
    traverse f (Pi x t ann) = rot3 Pi <$> f ann <*> traverse f x <*> traverse f t
    traverse f (App t1 t2 ann) = rot3 App <$> f ann <*> traverse f t1 <*> traverse f t2
    traverse f (Var x ann) = flip Var <$> f ann <*> pure x
    traverse f Type = pure Type
    traverse f Kind = pure Kind

instance Functor (Expr id) where
    fmap = fmapDefault

instance Foldable (Expr id) where
    foldMap = foldMapDefault

instance Traversable (TVar id) where
    traverse f (x ::: ty) = (:::) <$> pure x <*> traverse f ty

instance Functor (TVar id) where
    fmap = fmapDefault

instance Foldable (TVar id) where
    foldMap = foldMapDefault

-- | Map all annotations in the expression.
mapAnnot :: (a -> b) -> Expr id a -> Expr id b
mapAnnot = fmap

-- | The annotation on the root of the expression.
annot :: Expr id a -> a
annot = head . toList

-- | Annotation operator.
(%%) :: (a -> Expr id a) -> a -> Expr id a
(%%) = ($)

-- | Apply annotations to an annotation expecting context.
(%%%) :: ([a] -> Expr id a) -> [a] -> Expr id a
(%%%) = ($)

-- | Turn nested applications into a list.
unapply :: (Eq a, Eq id) => Expr id a -> [Expr id a]
unapply = reverse . aux where
    aux (App t1 t2 _) = t2 : aux t1
    aux t = [t]
