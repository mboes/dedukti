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
import qualified Europa.CodeGen.Exts as CG
import qualified Europa.Rule as Rule
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import Control.Monad (ap)


dump :: Module -> B.ByteString -> EuM ()
dump mod = io . B.writeFile (objPathFromModule mod)

-- | Emit Haskell code for one module.
compile :: Module -> EuM ()
compile mod = do
  say Verbose $ text "Parsing" <+> text (show mod) <+> text "..."
  let path = srcPathFromModule mod
  compileAST mod =<< return (parse path) `ap` io (B.readFile path)

compileAST :: Module -> ([Pa Binding], [Pa TyRule]) -> EuM ()
compileAST mod src@(decls, rules) = do
  say Verbose $ text "Compiling" <+> text (show mod) <+> text "..."
  let code = map CG.emit (Rule.ruleSets decls rules) :: [CG.Code]
      deps = collectDependencies src
  dump mod $ CG.serialize mod deps $ CG.coalesce code
