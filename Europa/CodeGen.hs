-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Interface for all code generators.
module Europa.CodeGen (CodeGen(..)) where

import Europa.Core
import Europa.Module
import qualified Data.ByteString.Lazy.Char8 as B


class CodeGen o where
    data Bundle o

    -- | Emit code corresponding to an individual rule set.
    emit :: RuleSet (Id o) (A o) -> o

    coalesce :: [o] -> Bundle o

    -- | Produce the byte sequence to write to a file, given the code
    -- for all the rule sets.
    serialize :: MName   -- ^ The module name
              -> [MName] -- ^ Dependencies
              -> Bundle o -- ^ Code
              -> B.ByteString

    -- | Produce interface code.
    interface :: MName -> Bundle o -> B.ByteString
