-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL

module Dedukti.Driver.Interactive (eval) where

import Dedukti.Module
import Dedukti.Parser
import qualified Dedukti.CodeGen.Exts as CG
import qualified Dedukti.Rule as Rule
import Data.ByteString.Lazy (ByteString)


dk = QuasiQuote { quoteExp = parse "<interactive>"
                , quoteDec = undefined }

-- | Emit Haskell code for one module.
eval :: String -> ByteString
eval input =
    let (decls, rules) = parse "<interactive>" input
        code = map CG.emit (Rule.ruleSets decls rules) :: [CG.Code]
        mod = Module (hierarchy ["interactive"])
    in CG.serialize (undefined :: CG.Code) mod $ CG.coalesce code
