-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Parser for an alternative format for Dedukti source files using reverse
-- polish notation. This format has the advantage of being very easy to parse
-- efficiently.

{-# OPTIONS_GHC -funbox-strict-fields #-}
module Dedukti.Parser.Prefix (Pa, parse) where

import {-# SOURCE #-} Dedukti.Parser
import Dedukti.Core
import Dedukti.Module
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Either (partitionEithers)
import Data.Char (isSpace)


type Token = B.ByteString
data Frame = Expr !(Pa Expr)
           | Binding !(Pa Binding)
           | Env [Pa Binding]
           | TyRule !(Pa TyRule)

type Stack = [Frame]


parse :: SourceName -> B.ByteString -> Pa Module
parse name = partitionEithers . map classify . foldr step [] . tokens
  where classify (Binding x) = Left x
        classify (TyRule x) = Right x
        classify _ = error $ "Parse error in " ++ name

tokens :: B.ByteString -> [Token]
tokens = filter (not . B.null) . B.splitWith isSpace

step :: Token -> Stack -> Stack

-- bindings
step ":" (Expr (V x _) : Expr y : xs) = Binding (x ::: y) : xs

-- rules
step "-->" (Env x : Expr y : Expr z : xs) = TyRule (fromBindings x :@ y :--> z) : xs

-- environments
step "," (Binding x : Env y : xs) = Env (x:y) : xs
step "[]" xs = Env [] : xs

-- expressions
step "=>"   (Binding (x ::: _) : Expr t : xs) = Expr (B (L x) t %% nann) : xs
step "->"   (Binding (x ::: ty) : Expr t : xs) = Expr (B (x ::: ty) t %% nann) : xs
step "@"    (Expr t1 : Expr t2 : xs)    = Expr (A t1 t2 %% nann) : xs
step "Type" xs                        = Expr Type : xs
step v xs = case reverse (B.split '.' v) of
  var : quals -> let mod = hierarchy (reverse quals)
                 in Expr (V (qualify mod (qid var)) %% nann) : xs
