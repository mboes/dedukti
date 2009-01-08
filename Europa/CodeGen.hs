module Europa.CodeGen (Variant(..), CodeGen(..)) where

import Europa.Core
import Europa.Module
import qualified Data.ByteString as B

data Variant = VtObject | VtType | VtSort
               deriving (Show, Eq)

class CodeGen o where
    type Id o
    type A o
    type Bundle o

    -- | Emit code corresponding to an individual rule set.
    emit :: Variant -> RuleSet (Id o) (A o) -> o

    coalesce :: [o] -> Bundle o

    -- | Produce the byte sequence to write to a file, given the code
    -- for all the rule sets.
    -- XXX: first argument is a temp hack to get around type families
    -- ambiguities.
    serialize :: o -> Module -> Bundle o -> B.ByteString

    -- | Produce interface code.
    interface :: Module -> Bundle o -> B.ByteString
