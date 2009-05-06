module Europa.Driver.Compile (compile) where

import Europa.Module
import Europa.Parser
import Europa.EuM
import Europa.Core
import qualified Europa.CodeGen.Exts as CG
import qualified Europa.Rule as Rule
import qualified Data.ByteString as B
import System.IO
import Control.Monad (ap)


dump :: Module -> B.ByteString -> EuM ()
dump mod = io . B.writeFile (objPathFromModule mod)

-- | Emit Haskell code for one module.
compile :: Module -> EuM ()
compile mod = do
  say Verbose $ text "Compiling" <+> text (show mod) <+> text "..."
  let path = srcPathFromModule mod
  (decls, rules) <- return (parse path) `ap` io (readFile path)
  let code = map CG.emit (Rule.ruleSets decls rules) :: [CG.Code]
  dump mod $ CG.serialize (undefined :: CG.Code) mod $ CG.coalesce code
