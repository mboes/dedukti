module Europa.Driver.Compile (compile) where

import Europa.Module
import Europa.Parser
import Europa.EuM
import Europa.Core
import qualified Europa.CodeGen.Exts as CG
import qualified Language.Haskell.Exts.Syntax as Hs
import qualified Europa.Rule as Rule
import qualified System.FilePath as FP
import System.IO
import Control.Monad (ap)
import qualified Data.ByteString as B


dump :: Module -> B.ByteString -> EuM ()
dump mod = liftIO . B.writeFile (objPathFromModule mod)

-- | Emit Haskell code for one module.
compile :: Module -> EuM ()
compile mod = do
  let path = srcPathFromModule mod
  (decls, rules) <- return (parse path) `ap` liftIO (readFile path)
  let code = foldr emits [] (Rule.ruleSets decls rules) :: [CG.Code]
  dump mod $ CG.serialize (undefined :: CG.Code) mod $ CG.coalesce code
    where emits x xs =   CG.emit CG.VtObject x
                       : CG.emit CG.VtType x
                       : CG.emit CG.VtSort x : xs
