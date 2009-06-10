-- |
-- Copyright : (c) 2009 INRIA
-- License   : GPL
--
-- Interface for all code generators.
module Europa.CodeGen (CodeGen(..)) where

import Europa.Core
import Europa.Module
import qualified Data.ByteString.Lazy.Char8 as B


class CodeGen o where
    type Id o
    type A o
    data Bundle o

    -- | Emit code corresponding to an individual rule set.
    emit :: RuleSet (Id o) (A o) -> o

    coalesce :: [o] -> Bundle o

    -- | Produce the byte sequence to write to a file, given the code
    -- for all the rule sets.
    serialize :: Module   -- ^ The module name
              -> [Module] -- ^ Dependencies
              -> Bundle o -- ^ Code
              -> B.ByteString

    -- | Produce interface code.
    interface :: Module -> Bundle o -> B.ByteString
