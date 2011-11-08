-- |
-- Copyright : © 2011 Mathieu Boespflug
-- License   : GPL
--
-- Normalize terms with respect to various reduction rules.

module Dedukti.Reduction (zeta) where

import Dedukti.Core


-- | Eliminate let forms.
zeta :: Ord id => Expr id a -> Expr id a
zeta = go [] where
  go subst (B (x := t') t _) = go ((x, go subst t'):subst) t
  go subst (B b t _) = go subst' t
    where subst' = filter ((bind_name b /=) . fst) subst
  go subst t@(V x _) | Just t' <- lookup x subst = t'
                     | otherwise = t
  go subst t = descend (go subst) t
