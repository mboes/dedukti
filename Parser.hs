module Parser (Pa, Parser.parse) where

import Text.Parsec
import Text.Parsec.Token
import Core
import Prelude hiding (pi)
import Control.Monad (ap)
import Control.Monad.Identity


-- The AST type as returned by the Parser.
type Pa t = t String Unannot

-- The parsing monad.
type P a = ParsecT String [Pa TyRule] Identity a

parse :: SourceName -> String -> Either ParseError ([Pa TVar], [Pa TyRule])
parse = runParser toplevel []

addRule :: Pa TyRule -> P ()
addRule rule = modifyState (rule:)

allRules :: P [Pa TyRule]
allRules = getState

lexDef = LanguageDef
         { commentStart = "(;"
         , commentEnd = ";)"
         , commentLine = ";"
         , nestedComments = False
         , identStart = letter
         , identLetter = alphaNum
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
  return (decls, rules)

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
    rhs <- applicative;
    dot lexer;
    addRule (env :@ lhs :--> rhs) } <?> "rule"

name = do
  ident <- qName <|> uName
  return $ Var ident nann

-- Qualified name.
qName = try (ident <?> "qualified identifier")
    where ident = do
            c <- identStart lexDef
            cs <- many (identLetter lexDef)
            let qualifier = c:cs
            char '.'
            c <- identStart lexDef
            cs <- many (identLetter lexDef)
            let name = c:cs
            return (qualifier ++ "." ++ name)

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
expression = do
  domain <- try (parens lexer complexBinding <|> simpleBinding)
            <|> return Hole `ap` applicative
  case domain of
    b@(x ::: ty) -> pi b <|> lambda b
    b@(Hole ty)  -> pi b <|> lambda b <|> return ty
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
