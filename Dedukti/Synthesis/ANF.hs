module Dedukti.Synthesis.ANF where

import qualified Dedukti.Core as Core
import Dedukti.Module
import Data.ByteString.Lazy.Char8 as B


-- | Core datatypes specialized to monadic form.
data Comp = Lam Qid Comp Comp
          | Pi Qid Comp Comp
          | App Atomic Atomic
          | Atomic Atomic
          | Let Qid Comp Comp

data Atomic = Var Qid
            | Type
            | Kind

data Binding = Qid ::: Comp
             | Hole Comp
             | Untyped Qid

bindingANF (x Core.::: ty) = (x, ty)
bindingANF (Core.Hole ty) = (qid "_" .$ "anf", ty)

-- | Identity translation for atomic forms.
atomic :: Core.Expr Qid a -> Comp
atomic (Core.Var x _) = Atomic (Var x)
atomic Core.Type = Atomic Type
atomic Core.Kind = Atomic Kind

monadicExpr :: Core.Expr Qid a -> Comp
monadicExpr = go 0 where
  -- Keep track of binding depth to generate fresh identifiers.
  go n (Core.Lam b e _) | (x, ty) <- bindingANF b = Lam x (go n ty) (go (n + 1) e)
  go n (Core.Pi b e _) | (x, ty) <- bindingANF b = Pi x (go n ty) (go (n + 1) e)
  go n (Core.App e1 e2 _) =
    let e1' = if Core.isAtomic e1 then atomic e1 else go n e1
        e2' = if Core.isAtomic e2 then atomic e2 else go n e2
        x = qid (B.append "x" (B.pack $ show n)) .$ "anf"
        y = qid (B.append "y" (B.pack $ show n)) .$ "anf"
    in Let x e1' (Let y e2' (App (Var x) (Var y)))

-- | Take the monadic calculus to A-normal form.
anf :: Comp -> Comp
anf (Let x e1 e2) = case anf e1 of
  Let y e1' e2' -> Let y e1' (anf (Let x e2' e2))
  e1' -> Let x e1' (anf e2)
