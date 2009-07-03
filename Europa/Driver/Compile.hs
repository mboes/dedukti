-- |
-- Copyright : (c) 2009 INRIA
-- License   : GPL
--
-- Compile one file to Haskell source code.

module Europa.Driver.Compile (compile, compileAST) where

import Europa.Module
import Europa.Parser
import Europa.EuM
import Europa.Core
import Europa.Analysis.Dependency
import Europa.Analysis.Scope
import qualified Europa.CodeGen.Exts as CG
import qualified Europa.Rule as Rule
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as Set
import Control.Monad (ap)


dump :: MName -> B.ByteString -> EuM ()
dump mod = io . B.writeFile (objPathFromModule mod)

-- | Qualify all occurrences of identifiers defined in current module.
selfQualify :: MName -> [RuleSet Qid a] -> [RuleSet Qid a]
selfQualify mod rsets = let defs = Set.fromList (map rs_name rsets)
                        in map (descend (f defs))
                               (map (\RS{..} -> RS{rs_name = rs_name{qid_qualifier = mod}, ..}) rsets)
    where f defs (Var x a) | Nothing <- provenance x
                           , x `Set.member` defs = Var x{qid_qualifier = mod} a
          f defs (Lam (x ::: ty) t a) =
              Lam (x ::: f defs ty) (f (Set.delete x defs) t) a
          f defs (Pi (x ::: ty) t a) =
              Pi (x ::: f defs ty) (f (Set.delete x defs) t) a
          f defs t = descend (f defs) (t :: Expr Qid a)

-- | Emit Haskell code for one module.
compile :: MName -> EuM ()
compile mod = do
  say Verbose $ text "Parsing" <+> text (show mod) <+> text "..."
  let path = srcPathFromModule mod
  compileAST mod =<< return (parse path) `ap` io (B.readFile path)

compileAST :: MName -> Pa Module -> EuM ()
compileAST mod src@(decls, rules) = do
  say Verbose $ text "Checking" <+> text (show mod) <+> text "..."
  checkUniqueness src
  checkScopes src
  checkRuleOrdering rules
  say Verbose $ pretty (concatMap rs_rules (Rule.ruleSets decls rules))
  say Verbose $ text "Compiling" <+> text (show mod) <+> text "..."
  let code = map CG.emit (selfQualify mod (Rule.ruleSets decls rules)) :: [CG.Code]
      deps = collectDependencies src
  dump mod $ CG.serialize mod deps $ CG.coalesce code
