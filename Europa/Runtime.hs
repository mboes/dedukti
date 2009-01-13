module Europa.Runtime
    ( Code(..), Term(..), ap
    , convertible
    , box, typeOf
    , runChecks) where

import Control.Monad hiding (ap)
import qualified Data.ByteString.Char8 as B
import Text.Show.Functions
import Data.Maybe (fromJust)
import Prelude hiding (pi)


data Code = Var !Int
          | Con !B.ByteString
          | Lam !(Code -> Code)
          | Pi  !Code !(Code -> Code)
          | App !Code !Code
          | Type
          | Kind
            deriving (Eq, Show)

instance Eq (Code -> Code)

data Term = TLam !Term !(Term -> Term)
          | TPi  !Term !(Term -> Term)
          | TApp !Term !Term
          | TType
          | Box Code Code
            deriving Show

ap :: Code -> Code -> Code
ap (Lam f) t = f t
ap t1 t2 = App t1 t2

obj :: Box -> Code
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

box :: Term -> Code -> Code -> Term
box ty ty_code obj_code | Just Type <- typeOf (-1) ty = Box ty_code obj_code
                        | Just Kind <- typeOf (-1) ty = Box ty_code obj_code
                        | otherwise = error "box exception."

typeOf :: Int -> Term -> Maybe Code
typeOf n (Box ty _) = return ty
typeOf n (TLam bx@(Box Type a) f) = do
  return $ Pi a (\x -> fromJust $ typeOf n (f (Box a x)))
typeOf n (TPi bx@(Box Type a) f) = do
  typeOf (incr n) (f (Box a (Var n)))
typeOf n (TApp t1 bx@(Box ty2 t2)) = do
  Pi tya f <- typeOf n t1
  guard (convertible (-1) tya ty2)
  return $ f t2
typeOf n TType = return Kind
typeOf n t = error (show t)

-- | Check that all items in the list are of sort Type or Kind.
runChecks :: [Maybe Code] -> IO ()
runChecks = undefined
-- runChecks = mapM_ check where
--     check (Just Type) = return ()
--     check (Just Kind)  = return ()
--     check _            = fail "Sort check failure."
