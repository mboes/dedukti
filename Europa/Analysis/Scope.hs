module Europa.Analysis.Scope where

import Europa.Core
import Europa.Module
import qualified Europa.Rule as Rule
import Europa.Pretty ()
import Europa.EuM
import Data.List (sort, group)
import qualified Data.Set as Set
import qualified Data.Map as Map


newtype DuplicateDefinition = DuplicateDefinition Qid
    deriving (Eq, Ord, Typeable)

instance Show DuplicateDefinition where
    show (DuplicateDefinition id) =
        show (text "duplicate definition" <+> pretty id)

instance Exception DuplicateDefinition

newtype NonContiguousRules = NonContiguousRules Qid
    deriving (Eq, Ord, Typeable)

instance Show NonContiguousRules where
    show (NonContiguousRules id) =
        show (text "Rules for" <+> pretty id <+> text "should be given contiguously.")

instance Exception NonContiguousRules

newtype ScopeError = ScopeError Qid
    deriving (Eq, Ord, Typeable)

instance Show ScopeError where
    show (ScopeError id) = show (pretty id <+> text "not in scope.")

instance Exception ScopeError

checkUniqueness :: [Binding Qid a] -> EuM ()
checkUniqueness decls = do
  mapM_ (\x -> when (length x > 1) (throw $ DuplicateDefinition (head x))) $
        group $ sort $ map (\(x ::: _) -> x) decls

checkRuleOrdering :: [TyRule Qid a] -> EuM ()
checkRuleOrdering rules = do
  mapM_ (\x -> when (length x > 1) (throw $ NonContiguousRules (head x))) $
        group $ sort $ map head $ group $ map Rule.headConstant rules

checkScopes :: forall a. Show a => Module Qid a -> EuM ()
checkScopes (decls, rules) = do
  mapM_ (descendM (f topenv)) decls
  mapM_ g rules
    where topenv = Set.fromList $ map (\(x ::: _) -> x) decls
          -- check declarations.
          f env t@(Var x _) = do
            when (x `Set.notMember` env) (throw $ ScopeError x)
            return (t :: Expr Qid a)
          f env (Lam (x ::: ty) t _) = f env ty >> f (Set.insert x env) t
          f env (Pi (x ::: ty) t _)  = f env ty >> f (Set.insert x env) t
          f env t = descendM (f env) t
          -- check rules.
          g (env :@ rule) = do
            let ruleenv = topenv `Set.union` Set.fromList (Map.keys env)
            descendM (f ruleenv) rule
            mapM_ (f ruleenv) (Map.elems env)
