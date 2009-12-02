-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Frontend to all the different parsers available. This module abstracts over
-- that fact that input can come in different formats, providing a single
-- point of entry to parsing instead.

module Dedukti.Parser where

import qualified Dedukti.Config as Config
import qualified Dedukti.Parser.External as External
import qualified Dedukti.Parser.Prefix as Prefix
import Dedukti.Core
import Dedukti.Module
import qualified Data.ByteString.Lazy.Char8 as B


type SourceName = String

-- The AST type as returned by the Parser.
type Pa t = t Qid Unannot

parse :: Config.Config -> SourceName -> B.ByteString -> Pa Module
parse config name input =
  let magic = "(; # FORMAT prefix # ;)\n" in
  case (magic `B.isPrefixOf` input, Config.format config) of
    (True, _) -> Prefix.parse name (B.drop (B.length magic) input)
    (False, Just Config.Prefix) -> Prefix.parse name input
    (False, Just Config.External) -> External.parse name input
