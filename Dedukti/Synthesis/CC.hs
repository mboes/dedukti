-- |
-- Copyright : © 2011 Mathieu Boespflug
-- License   : GPL
--
-- Closure conversion.

module Dedukti.Synthesis.CC where

import Dedukti.Core
import Dedukti.Module
import Control.Applicative ((<*>))
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as B


-- | Close all abstractions in a term.
closureConv :: Expr Qid Unannot -> Expr Qid Unannot
closureConv t = unabstract t (go (closed abstract)) where
  -- Check that the given term is closed at toplevel.
  closed k fvs | Set.null fvs = k
               | otherwise = \ _ t _ -> error $ "Term not closed: \n" ++ show (pretty t)
  go k bs t | isAbstraction t = \as ->
    let nas = repeat nann
        k' fvs bs' t1' as' =
          k (fvs Set.\\ Set.fromList (map bind_name bs)) bs
          (apply (abstract bxs (abstract bs' t1' %%% as') %%% nas) xs %%% nas) as
          where lfvs = Set.toList fvs; bxs = map L lfvs; xs = map V lfvs <*> nas
    in unabstract t (go k')
  go k bs (A t1 t2 a) = go (\fvs1 bs t1' ->
                                go (\fvs2 bs t2' ->
                                     k (fvs1 `Set.union` fvs2) bs (A t1' t2' a)) bs t2) bs t1
  -- If the variable is qualified then it is a constant of the global
  -- environment and should not be considered a free variable.
  go k bs (V x a) | Nothing <- provenance x = k (Set.singleton x) bs (V x a)
                  | otherwise = k Set.empty bs (V x a)
  go k bs t = k Set.empty bs t

-- | Replace every abstraction in the term with a placeholder variable and
-- float the abstraction to the top of the term.
hoist :: Expr Qid Unannot -> Expr Qid Unannot
hoist t = go (const abstract) 0 [] t %%% repeat nann
  where go k n lets t | isAbstraction t =
          let x = qid "l" .$ B.pack (show n) .$ "hoist"
          in unabstract t $ \bs t as ->
             go (\n lets t' -> k n (x := abstract bs t' as : lets) (V x %% nann)) (n + 1) lets t
        go k n lets (A t1 t2 a) =
          go (\n lets' t1' ->
               go (\n lets'' t2' ->
                    k n lets'' (A t1' t2' %% a)) n lets' t2) n lets t1
        go k n lets t = k n lets t
