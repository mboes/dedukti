-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Compile one file to Haskell source code.

module Dedukti.Driver.Compile (compile, compileAST) where

import Dedukti.Module
import Dedukti.Parser
import qualified Dedukti.Parser.Interface as Interface
import qualified Dedukti.Config as Config
import Dedukti.DkM
import Dedukti.Core
import Dedukti.Analysis.Dependency
import Dedukti.Analysis.Scope
import Control.Applicative
import qualified Dedukti.CodeGen as CG
import qualified Dedukti.CodeGen.Exts
--import qualified Dedukti.CodeGen.Lua
import qualified Dedukti.Rule as Rule
import qualified Dedukti.Analysis.Rule as Rule
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as Set


-- | Qualify all occurrences of identifiers defined in current
-- module. This gives us an easy way to distinguish constants,
-- belonging to the global environment, from variables, which are
-- locally bound in a term.
selfQualify :: MName -> [Pa RuleSet] -> [Pa RuleSet]
selfQualify mod rsets = let defs = Set.fromList (map rs_name rsets)
                        in map (descend (f defs))
                               (map (\RS{..} -> RS{rs_name = qualify mod rs_name, ..}) rsets)
    where f defs (V x a) | Nothing <- provenance x
                         , x `Set.member` defs = V (qualify mod x) %% a
          f defs (B (L x ty) t a) =
              B (L x (f defs `fmap` ty)) (f (Set.delete x defs) t) %% a
          f defs (B (x ::: ty) t a) =
              B (x ::: f defs ty) (f (Set.delete x defs) t) %% a
          f defs t = descend (f defs) (t :: Pa Expr)

-- | Read the interface file of each module name to collect the declarations
-- exported by the module.
populateInitialEnvironment :: [MName] -> DkM Context
populateInitialEnvironment deps =
    initContext . concat <$>
    mapM (\dep -> let path = ifacePathFromModule dep
                  in map (qualify dep) . Interface.parse path <$>
                     io (B.readFile path)) deps

-- | Generate the content of an interface file.
interface :: Pa Module -> B.ByteString
interface (decls, _) = B.unlines (map (fromAtom . qid_stem . bind_name) decls)

-- | Emit Haskell code for one module.
compile :: MName -> DkM ()
compile mod = do
  say Verbose $ text "Parsing" <+> text (show mod) <+> text "..."
  let path = srcPathFromModule mod
  config <- configuration
  compileAST mod =<< return (parse config path) `ap` io (B.readFile path)

-- | Emit code for one module, starting from the AST.
compileAST :: MName -> Pa Module -> DkM ()
compileAST mod src@(decls, rules) = do
  let deps = collectDependencies src
  -- For the purposes of scope checking, it is necessary to load in the
  -- environment all the declarations from immediate dependencies. For this
  -- we read an interface file - much faster to parse than the actual
  -- dependencies themselves.
  say Verbose $ text "Populating environment for" <+> text (show mod) <+> text "..."
  extdecls <- populateInitialEnvironment deps
  say Verbose $ text "Checking" <+> text (show mod) <+> text "..."
  checkUniqueness src
  checkScopes extdecls src
  Rule.checkOrdering rules
  say Verbose $ text "Checking well formation of rule heads ..."
  mapM_ Rule.checkHead rules
  say Debug $ pretty $ Rule.ruleSets decls rules
  say Verbose $ text "Compiling" <+> text (show mod) <+> text "..."
  let rs = selfQualify mod (Rule.ruleSets decls rules)
  parameter Config.cg >>= \cg -> case cg of
    Just _ -> undefined
    Nothing -> goCG (undefined :: Dedukti.CodeGen.Exts.Code) mod rs deps
  io $ B.writeFile (ifacePathFromModule mod) $ interface src
  where goCG :: forall g. CG.CodeGen g => g -> MName -> [RuleSet (Id g) (A g)] -> [MName] -> DkM ()
        goCG _ mod rs deps = do
          let code = map CG.emit rs :: [g]
          io $ B.writeFile (objPathFromModule mod) $ CG.serialize mod deps $ CG.coalesce code

