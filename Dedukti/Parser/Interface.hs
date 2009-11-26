-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Parser for interface files.

module Dedukti.Parser.Interface (parse) where

import Dedukti.Module
import qualified Data.ByteString.Lazy.Char8 as B

parse :: FilePath -> B.ByteString -> [Qid]
parse _ = map qid . B.lines

