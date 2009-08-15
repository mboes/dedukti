-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
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
import qualified Europa.Analysis.Rule as Rule
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as Set


readT = io . liftM T.decodeUtf8 . B.readFile
writeT path = io . B.writeFile path . T.encodeUtf8

-- | Qualify all occurrences of identifiers defined in current module.
selfQualify :: MName -> [Pa RuleSet] -> [Pa RuleSet]
selfQualify mod rsets = let defs = Set.fromList (map rs_name rsets)
                        in map (descend (f defs))
                               (map (\RS{..} -> RS{rs_name = rs_name{qid_qualifier = mod}, ..}) rsets)
    where f defs (Var x a) | Nothing <- provenance x
                           , x `Set.member` defs = Var x{qid_qualifier = mod} a
          f defs (Lam (x ::: ty) t a) =
              Lam (x ::: f defs ty) (f (Set.delete x defs) t) a
          f defs (Pi (x ::: ty) t a) =
              Pi (x ::: f defs ty) (f (Set.delete x defs) t) a
          f defs t = descend (f defs) (t :: Pa Expr)

-- | Read the interface file of each module name to collect the declarations
-- exported by the module.
populateInitialEnvironment :: [MName] -> EuM (Set.Set Qid)
populateInitialEnvironment =
    liftM Set.unions .
    mapM (\dep -> let path = ifacePathFromModule dep
                  in liftM (Set.fromList . map (qual dep) . parseIface path) $
                     readT path)
        where qual mod qid = qid{qid_qualifier = mod}

-- | Generate the content of an interface file.
interface :: Pa Module -> T.Text
interface (decls, _) = T.unlines (map (qid_stem . bind_name) decls)

-- | Emit Haskell code for one module.
compile :: MName -> EuM ()
compile mod = do
  say Verbose $ text "Parsing" <+> text (show mod) <+> text "..."
  let path = srcPathFromModule mod
  compileAST mod =<< return (parse path) `ap` readT path

-- | Emit Haskell code for one module, starting from the AST.
compileAST :: MName -> Pa Module -> EuM ()
compileAST mod src@(decls, rules) = do
  let deps = collectDependencies src
  -- For the purposes of scope checking it is necessary to load in the
  -- environment all those declarations from immediate dependencies. For this
  -- we read an interface file, much faster to parse than the actual
  -- dependencies themselves.
  say Verbose $ text "Populating environment for" <+> text (show mod) <+> text "..."
  extdecls <- populateInitialEnvironment deps
  say Verbose $ text "Checking" <+> text (show mod) <+> text "..."
  checkUniqueness src
  checkScopes extdecls src
  Rule.checkOrdering rules
  mapM_ Rule.checkHead rules
  say Debug $ pretty (concatMap rs_rules (Rule.ruleSets decls rules))
  say Verbose $ text "Compiling" <+> text (show mod) <+> text "..."
  let code = map CG.emit (selfQualify mod (Rule.ruleSets decls rules)) :: [CG.Code]
  writeT (objPathFromModule mod) $ CG.serialize mod deps $ CG.coalesce code
  writeT (ifacePathFromModule mod) $ interface src
