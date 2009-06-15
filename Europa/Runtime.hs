-- Copyright (c) 2009 INRIA
--
-- Permission to use, copy, modify, and distribute this file for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-- | All generated Haskell files import this module. The data type
-- declarations are given here, along with the conversion relation and type
-- inference function.

module Europa.Runtime
    ( Code(..), Term(..), ap
    , convertible
    , bbox, sbox, obj
    , checkDeclaration
    , checkRule) where

import qualified Data.ByteString.Char8 as B
import Control.Exception
import Text.Show.Functions ()
import Data.Typeable hiding (typeOf)
import Prelude hiding (pi, catch)
import System.IO


-- Exceptions

data SortError = SortError
    deriving (Show, Typeable)

data TypeError = TypeError
    deriving (Show, Typeable)

data RuleError = RuleError
    deriving (Show, Typeable)

instance Exception SortError
instance Exception TypeError
instance Exception RuleError

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
          | UBox Term Code
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

-- | A box in which we didn't put anything.
emptyBox = Box undefined undefined

bbox, sbox :: Term -> Code -> Code -> Term

-- | A big box holds terms of sort Type or Kind
bbox = box [Type, Kind]

-- | A small box holds terms of sort Type.
sbox = box [Type]

box sorts ty ty_code obj_code
    | typeOf 1000 ty `elem` sorts = Box ty_code obj_code
    | otherwise = throw SortError

typeOf :: Int -> Term -> Code
typeOf n (Box ty _) = ty
typeOf n (TLam bx@(Box Type a) f) = Pi a (\x -> typeOf n (f (Box a x)))
typeOf n (TPi bx@(Box Type a) f) = typeOf (incr n) (f (Box a (Var n)))
typeOf n (TApp t1 bx@(Box ty2 t2))
    | Pi tya f <- typeOf n t1, convertible (-1) tya ty2 = f t2
typeOf n (TApp t1 bx@(UBox tty2 t2))
    | Pi tya f <- typeOf n t1, ty2 <- typeOf n tty2,
      convertible (-1) tya ty2 = f t2
typeOf n TType = Kind
typeOf n t = throw TypeError

checkDeclaration :: String -> Term -> IO ()
checkDeclaration x t = catch (evaluate t >> putStrLn "Check") handler
    where handler (SomeException e) = do
            putStrLn $ "Error during checking of " ++ x
            throw e

checkRule :: Code -> Term -> Term -> Term
checkRule ty lhs rhs | convertible (-1) (typeOf 0 lhs) (typeOf 0 rhs) = emptyBox
                     | otherwise = throw RuleError
