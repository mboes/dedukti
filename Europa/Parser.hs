module Europa.Parser (Pa, Europa.Parser.parse) where

import Europa.Core
import Text.Parsec hiding (ParseError, parse)
import Text.Parsec.Token
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as Map
import qualified Control.Exception as Exception
import Data.Typeable (Typeable)


-- The AST type as returned by the Parser.
type Pa t = t String Unannot

-- The parsing monad.
type P = Parsec String [Pa TyRule]

newtype ParseError = ParseError String
    deriving Typeable

instance Show ParseError where
    show (ParseError e) = e

instance Exception.Exception ParseError

parse :: SourceName -> String -> ([Pa TVar], [Pa TyRule])
parse name input =
    case runParser ((,) <$> toplevel <*> allRules) [] name input of
      Left e -> Exception.throw (ParseError (show e))
      Right x -> x

addRule :: Pa TyRule -> P ()
addRule rule = modifyState (rule:)

allRules :: P [Pa TyRule]
allRules = getState

lexDef = LanguageDef
         { commentStart = "(;"
         , commentEnd = ";)"
         , commentLine = ";"
         , nestedComments = False
         , identStart = alphaNum <|> char '_' <|> char '\''
         , identLetter = alphaNum <|> char '_' <|> char '\''
         , opStart = parserFail "No user defined operators yet."
         , opLetter = parserFail "No user defined operators yet."
         , reservedNames = ["Type", "Kind"]
         , reservedOpNames = [":", "=>", "->", "-->"]
         , caseSensitive = True
         }

lexer :: TokenParser st
lexer = makeTokenParser lexDef

op = reservedOp lexer

-- | Qualified or unqualified name.
--
-- > qid ::= id.id | id
variable = ident <?> "qid" where
    ident = do
      c <- identStart lexDef
      cs <- many (identLetter lexDef)
      x <- (do let qualifier = c:cs
               c <- try $ do char '.'; identStart lexDef
               cs <- many (identLetter lexDef)
               let name = c:cs
               return (qualifier ++ "." ++ name))
           <|> return (c:cs)
      whiteSpace lexer
      return (Var x nann)

-- | Root production rule of the grammar.
--
-- > toplevel ::= declaration toplevel
-- >            | rule toplevel
-- >            | eof
toplevel =
    whiteSpace lexer *>
    (    (rule *> toplevel) -- Rules are accumulated by side-effect.
     <|> ((:) <$> declaration <*> toplevel)
     <|> (eof *> return []))

-- | Binding construct.
--
-- > binding ::= id : term
binding = ((:::) <$> try (identifier lexer <* op ":") <*> term)
          <?> "binding"

-- | Top-level declarations.
--
-- > declaration ::= id ":" term "."
declaration = (binding <* dot lexer)
              <?> "declaration"

-- | Left hand side of an abstraction or a product.
--
-- > domain ::= id ":" applicative
-- >          | applicative
domain = (    ((:::) <$> try (identifier lexer <* op ":") <*> applicative)
          <|> (Hole <$> applicative))
         <?> "domain"

-- |
-- > sort ::= Type
sort = Type <$ reserved lexer "Type"

-- | Terms and types.
--
-- We first try to parse as the domain of a lambda or pi. If we
-- later find out there was no arrow after the domain, then we take
-- the domain to be an expression, and return that.
--
-- > term ::= domain "->" term
-- >        | domain "=>" term
-- >        | applicative
term =     pi
       <|> lambda
       <|> applicative
    where pi = try (Pi <$> domain <* op "->" <*> term <%%> nann)
               <?> "pi"
          lambda = try (Lam <$> domain <* op "=>" <*> term <%%> nann)
                   <?> "lambda"

-- | Constituents of an applicative form.
--
-- > simple ::= sort
-- >          | qid
-- >          | "(" term ")"
simple = sort <|> variable <|> parens lexer term

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
-- > rule ::= env term "-->" term
-- > env ::= "[]"
-- >       | "[" env2 "]"
-- > env2 ::= binding
-- >        | binding "," env2
rule = ((\env lhs rhs -> foldl (&) Map.empty env :@ lhs :--> rhs)
        <$> brackets lexer (sepBy binding (comma lexer))
        <*> term
        <*  op "-->"
        <*> term
        <*  dot lexer) >>= addRule
       <?> "rule"
