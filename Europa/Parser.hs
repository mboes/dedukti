module Europa.Parser (Pa, Europa.Parser.parse) where

import Text.Parsec hiding (ParseError)
import Text.Parsec.Token
import Europa.Core
import Prelude hiding (pi)
import Control.Monad (ap)
import Control.Monad.Identity
import qualified Control.Exception as Exception
import qualified Data.Map as Map
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
parse name input = case runParser toplevel [] name input of
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
         , identStart = alphaNum <|> char '_'
         , identLetter = alphaNum <|> char '_'
         , opStart = parserFail "No user defined operators yet."
         , opLetter = parserFail "No user defined operators yet."
         , reservedNames = ["Type", "Kind"]
         , reservedOpNames = [":", "=>", "->", "-->"]
         , caseSensitive = True
         }

lexer :: TokenParser st
lexer = makeTokenParser lexDef

toplevel = do
  whiteSpace lexer
  decls <- declarations
  rules <- getState
  return (decls, reverse rules)

declarations =
      (do rule; declarations) --  Rules are accumulated by side-effect.
  <|> (do eof; return [])
  <|> (do b <- complexBinding
          dot lexer
          ds <- declarations
          return (b:ds))

-- | A binding whose type is an applicative term
simpleBinding = binding True

-- | A binding of arbitrary type.
complexBinding = binding False

binding simple = do
  { ident <- identifier lexer;
    reservedOp lexer ":";
    exp <- if simple then applicative else expression;
    return (ident ::: exp) } <?> "binding"

rule = do
  { env <- brackets lexer (sepBy complexBinding (comma lexer));
    lhs <- applicative;
    reservedOp lexer "-->";
    rhs <- expression;
    dot lexer;
    addRule (foldl (&) Map.empty env :@ lhs :--> rhs) } <?> "rule"

-- Qualified or unqualified name.
name = ident <?> "identifier" where
    ident = do
      c <- identStart lexDef
      cs <- many (identLetter lexDef)
      x <- (do let qualifier = c:cs
               c <- try $ do char '.'; identStart lexDef
               cs <- many (identLetter lexDef)
               let name = c:cs
               return (qualifier ++ "." ++ name)) <|> return (c:cs)
      whiteSpace lexer
      return (Var x nann)

-- Unqualified name.
uName = identifier lexer

-- | Expressions that are either a name or an application of a
-- expression to one or more arguments.
applicative = do
  exps <- many1 (parens lexer expression <|> baseType <|> name)
  return $ case exps of
             [exp] -> exp
             f:args -> apply f args (repeat nann)

-- We first try to parse as the domain of a lambda or pi. If we later
-- find out there was no arrow after the domain, then we take the
-- domain to be an expression, and return that.
expression =
    try $ do
      { domain <- try (parens lexer complexBinding <|> simpleBinding)
                  <|> return Hole `ap` applicative;
        case domain of
          b@(x ::: ty) -> pi b <|> lambda b
          b@(Hole ty)  -> pi b <|> lambda b <|> return ty;
      } <|> applicative
    where pi domain = do
                reservedOp lexer "->"
                range <- expression
                return (Pi domain range %% nann)
          lambda domain = do
                   reservedOp lexer "=>"
                   range <- expression
                   return (Lam domain range %% nann)

baseType = choice [ reserved lexer "Type" >> return Type
                  , reserved lexer "Kind" >> return Kind ]
