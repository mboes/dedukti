-- |
-- Copyright : © 2011 Mathieu Boespflug
-- License   : GPL
--
-- Transformation to administrative normal form.

{-# LANGUAGE MagicHash, RankNTypes #-}
module Dedukti.Synthesis.ANF (monadic, anf, isANF) where

import Dedukti.Core
import Dedukti.Module
import Data.ByteString.Lazy.Char8 as B
import GHC.Exts

-- XXX: put this freshness monad somewhere else.
newtype Fresh a = Fresh { unFresh :: forall r. (a -> Int# -> r) -> Int# -> r }

instance Monad Fresh where
  return x = Fresh $ \k -> k x
  Fresh g >>= f = Fresh $ \k -> g (\a -> unFresh (f a) k)

runFresh :: Fresh a -> a
runFresh m = unFresh m (\x _ -> x) 0#

fresh :: Fresh Int
fresh = Fresh $ \k n -> k (I# n) (n +# 1#)

monadic :: Expr Qid Unannot -> Expr Qid Unannot
monadic = runFresh . go where
  -- Maintain a counter for generating fresh identifiers.
  go (B b t a) = do
    b' <- descendM go b
    t' <- go t
    return $ B b' t' %% a
  go (A t1 t2 a) = do
    n <- fresh
    t1' <- go t1
    t2' <- go t2
    let x = qid "x" .$ B.pack (show n) .$ "anf"
        y = qid "y" .$ B.pack (show n) .$ "anf"
        b1 k' = if isApplicative t1 then k' t1' else B (x := t1') (k' (V x %% annot t1)) a
        b2 k' = if isAtomic t2      then k' t2' else B (y := t2') (k' (V y %% annot t2)) a
    return $ b1 $ \x -> b2 $ \y -> A x y a
  go t = return t

-- | Take the monadic calculus to A-normal form.
anf :: Ord id => Expr id a -> Expr id a
anf (B (x := t1) t2 a) = case anf t1 of
  B (y := t3) t4 a' -> B (y := t3) (anf (B (x := t4) t2 %% a')) %% a
  t1' -> B (x := t1') (anf t2) %% a
anf t = descend anf t

-- | Sanity check.
isANF :: Ord id => Expr id a -> Bool
isANF x = and [test t | t <- everyone x] where
  test (A x y _) = isAtomic x && isAtomic y
  test (B (_ := (B (_ := _) _ _)) _ _) = False
  test _ = True
