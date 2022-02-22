module Parser (
  read,
) where

import Types

import Data.Text                  qualified as T
import Text.Megaparsec            qualified as P
import Text.Megaparsec.Char       qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Char (isLetter)
import Data.Text (Text)
import Data.Void (Void)
import Prelude hiding (read)
import Text.Megaparsec (empty, (<?>), (<|>))


type Parser = P.Parsec Void Text

read :: Text -> Either String Expr
read inp = case P.parse (pExpr <* P.eof) "" inp of
  Left err   -> Left $ P.errorBundlePretty err
  Right expr -> Right expr

pExpr :: Parser Expr
pExpr = pOps <|> P.try pApp <|> pLambda <|> pTerm

pInt :: Parser Expr
pInt = EInt <$> L.signed space L.decimal <?> "integer"

pVar :: Parser Var
pVar = lexeme (T.cons <$> P.letterChar <*> takeSymbol) <?> "variable"

pLambda :: Parser Expr
pLambda = optParens $ space *> (<?> "lambda") do
  _  <- symbol "\\"
  vs <- pVar `P.sepBy` space
  _  <- symbol "."
  e  <- lexeme pExpr
  pure $ ELam vs e

pApp :: Parser Expr
pApp = optParens $ (<?> "application") do
  lam <- "(" *> pLambda <* ")"
  apps <- space *> pExpr `P.sepBy1` space
  pure $ EApp lam apps

pTerm :: Parser Expr
pTerm = P.try ("(" *> pOps <* ")")
    <|> pInt
    <|> (EVar <$> pVar)

pOps :: Parser Expr
pOps = makeExprParser (lexeme pTerm) table <?> "binary operation"
 where
  table :: [[Operator Parser Expr]]
  table = [ [ binary "*" (EBin "*") ]
          , [ binary "+" (EBin "+")
            , binary "-" (EBin "-")
            ]
          ]
  binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
  binary name f = InfixL (f <$ symbol name)

takeSymbol :: Parser (P.Tokens Text)
takeSymbol = P.takeWhileP Nothing isLetter

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: P.Tokens Text -> Parser Text
symbol = L.symbol space

-- | We have no comments.
space :: Parser ()
space = L.space P.space1 empty empty

optParens :: Parser a -> Parser a
optParens p = P.try ("(" *> p <* ")") <|> p
