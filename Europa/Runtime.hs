module Europa.Runtime
    ( Code(..), Term(..), ap
    , convertible
    , box, typeOf
    , runChecks) where

import qualified Data.ByteString.Char8 as B
import Control.Monad hiding (ap)
import Control.Exception
import Text.Show.Functions
import Data.Typeable hiding (typeOf)
import Data.Maybe (fromJust)
import Prelude hiding (pi)


-- Exceptions

newtype  SortError = SortError Term
    deriving (Show, Typeable)

newtype TypeError = TypeError Term
    deriving (Show, Typeable)

instance Exception SortError
instance Exception TypeError

-- Convertible and static terms.

data Code = Var !Int
          | Con !B.ByteString
          | Lam !(Code -> Code)
          | Pi  Code !(Code -> Code)
          | App Code Code
          | Type
          | Kind
            deriving (Eq, Show)

data Term = TLam !Term !(Term -> Term)
          | TPi  !Term !(Term -> Term)
          | TApp !Term !Term
          | TType
          | Box Code Code
            deriving Show

instance Eq (Code -> Code)

ap :: Code -> Code -> Code
ap (Lam f) t = f t
ap t1 t2 = App t1 t2

obj :: Term -> Code
obj (Box _ obj) = obj

incr x = if x >= 0 then x + 1 else x - 1

convertible :: Int -> Code -> Code -> Bool
convertible n (Var x) (Var x') = x == x'
convertible n (Con c) (Con c') = c == c'
convertible n (Lam t) (Lam t') =
    convertible (incr n) (t (Var n)) (t' (Var n))
convertible n (Pi ty1 ty2) (Pi ty3 ty4) =
    convertible n ty1 ty3 && convertible (incr n) (ty2 (Var n)) (ty4 (Var n))
convertible n (App t1 t2) (App t3 t4) =
    convertible n t1 t3 && convertible n t2 t4
convertible n Type Type = True
convertible n Kind Kind = True
convertible n _ _ = False

bbox, sbox :: Term -> Code -> Code -> Term

-- | A big box holds terms of sort Type or Kind
bbox = box [Type, Kind]

-- | A small box holds terms of sort Type.
sbox = box [Type]

box sorts ty ty_code obj_code
    | typeOf (-1) ty `elem` sorts = Box ty_code obj_code
    | otherwise = throw (SortError ty)

typeOf :: Int -> Term -> Code
typeOf n (Box ty _) = ty
typeOf n (TLam bx@(Box Type a) f) = Pi a (\x -> typeOf n (f (Box a x)))
typeOf n (TPi bx@(Box Type a) f) = typeOf (incr n) (f (Box a (Var n)))
typeOf n (TApp t1 bx@(Box ty2 t2))
    | Pi tya f <- typeOf n t1, convertible (-1) tya ty2 = f t2
typeOf n TType = Kind
typeOf n t = throw (TypeError t)

-- | Check that all items in the list are of sort Type or Kind.
runChecks :: [Term] -> IO ()
runChecks = mapM_ (\(Box _ _) -> return ())
