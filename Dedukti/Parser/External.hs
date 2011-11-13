-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Dedukti.Parser.External (Pa, parse) where

import {-# SOURCE #-} Dedukti.Parser
import Dedukti.Core
import Dedukti.Module
import Text.Parsec hiding (SourceName, ParseError, parse)
import qualified Text.Parsec.Token as Token
import Control.Applicative hiding ((<|>), many)
import Control.Monad.Identity
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Exception as Exception
import Data.Typeable (Typeable)


-- The parsing monad.
type P = Parsec B.ByteString [Pa TyRule]

newtype ParseError = ParseError String
    deriving Typeable

instance Show ParseError where
    show (ParseError e) = e

instance Exception.Exception ParseError

newtype IfaceError = IfaceError String
    deriving Typeable

instance Show IfaceError where
    show (IfaceError f) = "Broken interface file " ++ f ++ "."

instance Exception.Exception IfaceError

parse :: SourceName -> B.ByteString -> Pa Module
parse name input =
    -- At the toplevel, a source file is a list of declarations and rule
    -- definitions. Here rules are accumulated by side-effect, added to the
    -- parser state as we encounter them.
    case runParser ((,) <$> toplevel <*> allRules) [] name input of
      Left e -> Exception.throw (ParseError (show e))
      Right x -> x

addRule :: Pa TyRule -> P ()
addRule rule = modifyState (rule:)

-- | Retrieve all rules encountered so far from the parser state.
allRules :: P [Pa TyRule]
allRules = liftM reverse getState

lexDef = Token.LanguageDef
         { Token.commentStart = "(;"
         , Token.commentEnd = ";)"
         , Token.commentLine = ";"
         , Token.nestedComments = False
         , Token.identStart = alphaNum <|> char '_' <|> char '\''
         , Token.identLetter = alphaNum <|> char '_' <|> char '\''
         , Token.opStart = parserFail "No user defined operators yet."
         , Token.opLetter = parserFail "No user defined operators yet."
         , Token.reservedNames = ["Type", "Kind"]
         , Token.reservedOpNames = [":", "=>", "->", "-->"]
         , Token.caseSensitive = True
         }

Token.LanguageDef{..} = lexDef
Token.TokenParser{..} = Token.makeTokenParser lexDef

-- | Qualified or unqualified name.
--
-- > qid ::= id.id | id
qident = ident <?> "qid" where
    ident = do
      c <- identStart
      cs <- many identLetter
      x <- (do let qualifier = B.pack (c:cs)
               c <- try $ do char '.'; identStart
               cs <- many identLetter
               let name = B.pack (c:cs)
               return $ qualify (hierarchy [qualifier]) (qid name))
           <|> return (qid (B.pack (c:cs)))
      whiteSpace
      return (V x nann)

-- | Unqualified name.
ident = qid . B.pack <$> identifier

-- | Root production rule of the grammar.
--
-- > toplevel ::= declaration toplevel
-- >            | rule toplevel
-- >            | eof
toplevel =
    whiteSpace *>
    (    (rule *> toplevel) -- Rules are accumulated by side-effect.
     <|> ((:) <$> declaration <*> toplevel)
     <|> (eof *> return []))

-- | Binding construct.
--
-- > binding ::= id ":" term
binding = ((\x ty -> L x ::: ty) <$> ident <* reservedOp ":" <*> term)
          <?> "binding"

-- | Top-level declarations.
--
-- > declaration ::= binding "."
declaration = (binding <* dot)
              <?> "declaration"

-- | Left hand side of an abstraction or a product.
--
-- > domain ::= id ":" applicative
-- >          | applicative
domain = (    ((,) <$> try (ident <* reservedOp ":") <*> applicative)
          <|> ((qid "hole" .$ "parser",) <$> applicative))
         <?> "domain"

-- |
-- > sort ::= "Type"
sort = Type <$ reserved "Type"

-- | Terms and types.
--
-- We first try to parse as the domain of a lambda or pi. If we
-- later find out there was no arrow after the domain, then we take
-- the domain to be an expression, and return that.
--
-- > term ::= domain "->" term
-- >        | domain "=>" term
-- >        | applicative
term = do
  d@(x,ty) <- domain
  choice [ pi d <?> "pi"
         , lambda d <?> "lambda"
         , return ty ]
    where pi (x, ty)    = B (P x ::: ty) <$ reservedOp "->" <*> term <%%> nann
          lambda (x, _) = B (L x) <$ reservedOp "=>" <*> term <%%> nann

-- | Constituents of an applicative form.
--
-- > simple ::= sort
-- >          | qid
-- >          | "(" term ")"
simple = sort <|> qident <|> parens term

-- | Expressions that are either a name or an application of a
-- expression to one or more arguments.
--
-- > applicative ::= simple
-- >               | applicative simple
-- >
applicative = (\xs -> case xs of
                        [t] -> t
                        (f:ts) -> apply f ts (repeat nann))
              <$> many1 simple
              <?> "applicative"

-- | A rule.
--
-- > rule ::= env term "-->" term "."
-- > env ::= "[]"
-- >       | "[" env2 "]"
-- > env2 ::= binding
-- >        | binding "," env2
rule = ((\env lhs rhs -> foldr (&) emptyEnv env :@ lhs :--> rhs)
        <$> brackets (sepBy binding comma)
        <*> term
        <*  reservedOp "-->"
        <*> term
        <*  dot) >>= addRule
       <?> "rule"
