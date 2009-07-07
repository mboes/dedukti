-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL

module Europa.Driver.Interactive (eval) where

import Europa.Module
import Europa.Parser
import qualified Europa.CodeGen.Exts as CG
import qualified Europa.Rule as Rule
import Data.ByteString.Lazy (ByteString)


-- | Emit Haskell code for one module.
eval :: String -> ByteString
eval input =
    let (decls, rules) = parse "<interactive>" input
        code = map CG.emit (Rule.ruleSets decls rules) :: [CG.Code]
        mod = Module (hierarchy ["interactive"])
    in CG.serialize (undefined :: CG.Code) mod $ CG.coalesce code
