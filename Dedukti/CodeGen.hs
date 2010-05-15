-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Interface for all code generators.

module Dedukti.CodeGen where

import Dedukti.Core
import Dedukti.Module
import Data.ByteString.Lazy.Char8 (ByteString)


class CodeGen o where
    data Bundle o

    -- | Emit code corresponding to an individual rule set.
    emit :: RuleSet (Id o) (A o) -> o

    -- | Combine code from a number of rule sets, typically forming a module,
    -- into a bundle.
    coalesce :: [o] -> Bundle o

    -- | Produce the byte sequence to write to a file, given the code
    -- for all the rule sets.
    serialize :: MName   -- ^ The module name
              -> [MName] -- ^ Dependencies
              -> Bundle o -- ^ Code
              -> ByteString
