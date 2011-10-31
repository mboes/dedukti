-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Check that all occurrences of variables are in scope of their definitions.
-- Other well-formedness checks can also be found here, such as rejecting
-- duplicate top-level definitions.

module Dedukti.Analysis.Scope where

import Dedukti.Core
import Dedukti.Module
import qualified Dedukti.Rule as Rule
import Dedukti.Pretty ()
import Dedukti.DkM
import Data.List (sort, group)
import qualified Data.Map as Map
import qualified StringTable.AtomSet as AtomSet


newtype DuplicateDefinition = DuplicateDefinition Qid
    deriving (Eq, Ord, Typeable)

instance Show DuplicateDefinition where
    show (DuplicateDefinition id) =
        show (text "duplicate definition" <+> pretty id)

instance Exception DuplicateDefinition

newtype ScopeError = ScopeError Qid
    deriving (Eq, Ord, Typeable)

instance Show ScopeError where
    show (ScopeError id) = show (pretty id <+> text "not in scope.")

instance Exception ScopeError

newtype IllegalEnvironment = IllegalEnvironment Qid
    deriving (Eq, Ord, Typeable)

instance Show IllegalEnvironment where
    show (IllegalEnvironment id) = show (pretty id <+> text "appears in environment but not in head of rule.")

instance Exception IllegalEnvironment

-- | Check that top-level constants are declared only once.
checkUniqueness :: Module Qid a -> DkM ()
checkUniqueness (decls, rules) = do
  say Verbose $ text "Checking that constants are declared only once ..."
  chk decls
  mapM_ (\(env :@ _) -> chk (env_bindings env)) rules
    where chk bs = mapM_ (\x -> when (length x > 1)
                                (throw $ DuplicateDefinition (head x))) $
                   group $ sort $ map bind_name bs

type Context = Map.Map MName AtomSet.AtomSet

-- | Initial environment to pass to 'checkScopes'.
initContext :: [Qid]                -- ^ declarations from other modules.
            -> Context
initContext qids =
  Map.fromListWith AtomSet.union $ map (\qid -> (qid_qualifier qid, AtomSet.singleton (qid_stem qid))) qids

-- | Check that all variables and constants are well scoped.
checkScopes :: Context -> Module Qid a -> DkM ()
checkScopes env (decls, rules) = do
  say Verbose $ text "Checking that all declarations are well scoped ..."
  topenv <- foldM chkBinding env decls
  mapM_ (chkRule topenv) rules
    where ins qid env = Map.insertWith' AtomSet.union (qid_qualifier qid)
                        (AtomSet.singleton (qid_stem qid)) env
          notmem qid env = maybe False (AtomSet.notMember (qid_stem qid))
                        (Map.lookup (qid_qualifier qid) env)
          chkBinding env (L x) = return $ ins x env
          chkBinding env (x ::: ty) = do
            chkExpr env ty
            return $ ins x env
          chkBinding env (x := t) = do
            chkExpr env t
            return $ ins x env
          chkRule topenv r@(env :@ rule) = do
            let lhsvars = AtomSet.fromList [ qid_stem x | V x _ <- everyone (Rule.head r) ]
            mapM_ (\x -> when (qid_stem x `AtomSet.notMember` lhsvars) $
                         throw (IllegalEnvironment x)) (map bind_name $ env_bindings env)
            ruleenv <- foldM chkBinding topenv $ env_bindings env
            descendM (chkExpr (Map.unionWith AtomSet.union topenv ruleenv)) rule
          chkExpr env t@(V x _) = do
            when (x `notmem` env) (throw $ ScopeError x)
            return t
          chkExpr env (B (L x) t _) = do
            chkExpr (ins x env) t
          chkExpr env (B (x ::: ty) t _)  = do
            chkExpr env ty
            chkExpr (ins x env) t
          chkExpr env t = descendM (chkExpr env) t
