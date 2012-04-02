-- Copyright © 2009 CNRS - École Polytechnique - INRIA.
-- Copyright © 2011 Mathieu Boespflug.
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

module Dedukti.Runtime
    ( Code(..), Term(..), ap
    , reflect
    , convertible
    , bbox, sbox, obj
    , start, stop
    , checkDeclaration
    , checkRule) where

import qualified Data.ByteString.Char8 as B
import Control.Exception
import Text.Show.Functions ()
import Data.Typeable
import Prelude hiding (pi, catch)
import Data.Time.Clock
import Text.PrettyPrint.Leijen
import System.Exit
import System.Posix.Process (exitImmediately)


-- Exceptions

data SortError = SortError
    deriving (Show, Typeable)

data CheckError = CheckError
    deriving (Show, Typeable)

data SynthError = SynthError
    deriving (Show, Typeable)

data ConvError = ConvError Doc Doc
    deriving (Show, Typeable)

data RuleError = RuleError
    deriving (Show, Typeable)

instance Exception SortError
instance Exception CheckError
instance Exception SynthError
instance Exception ConvError
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

data Term = TLam !(Term -> Term)
          | TPi  !Term !(Term -> Term)
          | TLet !Term !(Term -> Term)
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

-- | In the runtime, predicates are represented in half CPS, taking a success
-- continuation but throw an exception on failure.
type Prop = forall r. r -> r

-- | Check that a predicate really evaluates to True.
reflect :: Prop -> Bool
reflect k | r <- k True = r

convertible :: Int -> Code -> Code -> Prop
convertible n t1 t2 k | conv n t1 t2 = k
                      | otherwise = throw $ ConvError (prettyOpen n t1) (prettyOpen n t2)
  where conv n (Var x) (Var x') = x == x'
        conv n (Con c) (Con c') = c == c'
        conv n (Lam t) (Lam t') =
          conv (n + 1) (t (Var n)) (t' (Var n))
        conv n (Pi ty1 ty2) (Pi ty3 ty4) =
          conv n ty1 ty3 && conv (n + 1) (ty2 (Var n)) (ty4 (Var n))
        conv n (App t1 t2) (App t3 t4) =
          conv n t1 t3 && conv n t2 t4
        conv n Type Type = True
        conv n Kind Kind = True
        conv n _ _ = False

-- | A box in which we didn't put anything.
emptyBox = Box undefined undefined

bbox, sbox :: Term -> Code -> Code -> Term

-- | A big box holds terms of sort Type or Kind
bbox = box [Type, Kind]

-- | A small box holds terms of sort Type.
sbox = box [Type]

box sorts ty ty_code obj_code
    | synth 0 ty `elem` sorts = Box ty_code obj_code
    | otherwise = throw SortError

check :: Int -> Term -> Code -> Prop
check n (TLam f) (Pi a f') = check (n + 1) (f (Box a (Var n))) (f' (Var n))
check n t ty = convertible n (synth n t) ty

synth :: Int -> Term -> Code
synth n (Box ty _) = ty
synth n (TPi (Box Type tya) f) = synth (n + 1) (f (Box tya (Var n)))
synth n (TApp t1 (Box ty2 t2))
    | Pi tya f <- synth n t1,
      reflect (convertible n tya ty2) = f t2
synth n (TLet t1 f) | ty <- synth n t1 = synth (n + 1) (f (Box ty (Var n)))
synth n TType = Kind
synth n t = throw SynthError

checkDeclaration :: String -> Term -> IO ()
checkDeclaration x t = catch (evaluate t >> putStrLn ("Checked " ++ x ++ ".")) handler
    where handler (SomeException e) = do
            putStrLn $ "Error during checking of " ++ x ++ "."
            throw e

checkRule :: Term -> Term -> Term
checkRule lhs rhs | ty <- synth 0 lhs, reflect (check 0 rhs ty) = emptyBox
                  | otherwise = throw $ RuleError

start :: IO UTCTime
start = do
  putStrLn "Start."
  getCurrentTime


stop :: UTCTime -> IO ()
stop t = do
  t' <- getCurrentTime
  let total = diffUTCTime t' t
  putStrLn $ "Stop. Runtime: " ++ show total
  -- Use Posix exitImmediately rather than System.Exit to really exit GHCi.
  exitImmediately ExitSuccess

-- Pretty printing.

instance Pretty Code where
  pretty = prettyOpen 0

prettyOpen n (Var x) = text (show x)
prettyOpen n (Con c) = text (show c)
prettyOpen n (Lam f) =
  parens (int n <+> text "=>" <+> prettyOpen (n + 1) (f (Var n)))
prettyOpen n (Pi ty1 ty2) =
  parens (int n <+> colon <+> prettyOpen n ty1 <+> text "->" <+> prettyOpen (n + 1) (ty2 (Var n)))
prettyOpen n (App t1 t2) =
  parens (prettyOpen n t1 <+> prettyOpen n t2)
prettyOpen n Type = text "Type"
prettyOpen n Kind = text "Kind"
