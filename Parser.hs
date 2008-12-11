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
  <|> (do b <- binding
          dot lexer
          ds <- declarations
          return (b:ds))

declaration = do
  optional rule

binding = do
  ident <- identifier lexer
  reservedOp lexer ":"
  exp <- expression
  return (ident ::: exp)

rule = do
  env <- brackets lexer (sepBy binding (comma lexer))
  lhs <- applicative
  reservedOp lexer "-->"
  rhs <- applicative
  dot lexer
  addRule (env :@ lhs :--> rhs)

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

applicative = do
  exps <- many1 (parens lexer expression <|> baseType <|> name)
  return $ case exps of
             [exp] -> exp
             f:args -> apply f args (repeat nann)

expression = (try lambda <?> "lambda expression") <|> do
  domain <- applicative
  option domain $ do
               reservedOp lexer "->"
               range <- expression
               return ((domain .-> range) %% nann)

lambda = do
  b <- binding <?> "binding"
  reservedOp lexer "=>"
  exp <- expression
  return (Lam b exp %% nann)

baseType = choice [ reserved lexer "Type" >> return Type
                  , reserved lexer "Kind" >> return Kind ]
