module Dedukti.Synthesis.ANF where

import Dedukti.Core
import Dedukti.Module
import Data.ByteString.Lazy.Char8 as B


type Af t = t Qid Unannot

type instance Id (Af t) = Qid
type instance A  (Af t) = Unannot

monadic :: Transform (Af t) => Af t -> Af t
monadic = descend (go 0) where
  -- Keep track of binding depth to generate fresh identifiers.
  go :: Int -> Af Expr -> Af Expr
  go n (B b t a) = B (descend (go n) b) (go (n + 1) t) %% a
  go n (A t1 t2 a) =
    let t1' = if isAtomic t1 then t1 else go n t1
        t2' = if isAtomic t2 then t2 else go n t2
        x = qid (B.append "x" (B.pack $ show n)) .$ "anf"
        y = qid (B.append "y" (B.pack $ show n)) .$ "anf"
    in B (x := t1') (B (y := t2') (A (V x %% annot t1) (V y %% annot t2) %% a) %% a) %% a
  go n e = e

-- | Take the monadic calculus to A-normal form.
anf :: Transform (Af t) => Af t -> Af t
anf = descend go where
  go (B (x := t1) t2 a) = case go t1 of
    B (y := t3) t4 a' -> B (y := t3) (go (B (x := t4) t2 %% a')) %% a
    t1' -> B (x := t1') (go t2) %% a
  go t = descend go t

-- | Sanity check.
isANF :: Transform (Af t) => Af t -> Bool
isANF x = and [test t | t <- everyone x] where
  test (A x y _) = isAtomic x && isAtomic y
  test (B (_ := (B (_ := _) _ _)) _ _) = False
  test _ = True
