module Europa.Runtime
    ( Code(..), Term(..), ap, tap
    , convertible, normalize
    , box, sortOf, typeOf
    , runChecks) where

import Control.Monad hiding (ap)
import qualified Data.ByteString.Char8 as B
import Text.Show.Functions


data Code = Var !Int
          | Con !B.ByteString
          | Lam !(Code -> Code)
          | App !Code !Code
            deriving Show

data Term = TLam !Term !(Term -> Term)
          | TPi  !Term !(Term -> Term)
          | TApp !Term !Term
          | TCon !B.ByteString
          | TBox !Term
          | TType
          | TKind
            deriving Show

ap :: Code -> Code -> Code
ap (Lam f) t = f t
ap t1 t2 = App t1 t2

tap :: Term -> Term -> Term
tap (TLam _ f) t = f t
tap t1 t2 = TApp t1 t2

convertible :: Int -> Code -> Code -> Bool
convertible n (Var x) (Var x') = x == x'
convertible n (Con c) (Con c') = c == c'
convertible n (Lam t) (Lam t') =
    convertible (n + 1) (t (Var n)) (t' (Var n))
convertible n (App t1 t2) (App t3 t4) =
    convertible n t1 t3 && convertible n t2 t4
convertible n _ _ = False

normalize :: Int -> Code -> Code
normalize n (Lam t) = normalize (n+1) (t (Var n))
normalize n (App t1 t2) = App t1 (normalize n t2)

sortOf :: Term -> Maybe Term
sortOf ty = do
  sort <- typeOf ty
  case sort of
    TType -> return TType
    TKind -> return TKind
    _ -> mzero

box :: Term -> Maybe Term
box ty = do sortOf ty; return $ TBox ty

-- We only rewrite inside boxes, and constants only appear as a consequence of
-- an application of a rewrite rule, so we don't need to type constants.
typeOf :: Term -> Maybe Term
-- rule: Sort
typeOf TType = return TKind
-- rule: Annotated term
-- Invariant: the type of a box has always been checked prior to its usage.
typeOf (TBox ty) = return ty
-- rules: Abstraction 1 and 2
typeOf (TLam ty t) = do
  TType <- typeOf ty
  return $ TPi ty t
-- rules: Product 1 and 2
typeOf (TPi tyd tyr) = do
  TType <- typeOf tyd
  sortOf (tyr (TBox tyd))
-- rule: Application
typeOf (TApp t1 t2) = do
  TPi tyd tyr <- typeOf t1
  t2ty <- typeOf t2
--  guard (convertible tyd t2ty)
  return (tyr (TBox tyd))
-- otherwise typeChecking fails
typeOf _ = mzero

-- | Check that all items in the list are of sort Type or Kind.
runChecks :: [Maybe Term] -> IO ()
runChecks = mapM_ check where
    check (Just TType) = return ()
    check (Just TKind)  = return ()
    check _            = fail "Sort check failure."
