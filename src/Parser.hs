module Parser where

import Types

import qualified Data.Text                  as T
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative
import Data.Char (isLetter)
import Data.Text (Text)
import Data.Void
import Prelude hiding (read)
import Control.Monad.Combinators.Expr


type Parser = P.Parsec Void Text

read :: Text -> Either String Expr
read inp = case P.parse (pExpr <* P.eof) "" inp of
  Left err   -> Left $ show err
  Right expr -> Right expr

pExpr :: Parser Expr
pExpr = pOps <|> pLambda <|> pApp <|> pTerm

pInt :: Parser Expr
pInt = EInt <$> L.signed space L.decimal

pVar :: Parser Var
pVar = lexeme (T.cons <$> P.letterChar <*> takeSymbol)

pLambda :: Parser Expr
pLambda = space *> do
  _  <- symbol "\\"
  vs <- pVar `P.sepBy` space
  _  <- symbol "."
  e  <- lexeme pExpr
  pure $ ELam vs e

pApp :: Parser Expr
pApp = do
  lam <- "(" *> pLambda <* ")"
  apps <- space *> pExpr `P.sepBy` space
  pure $ EApp lam apps

pTerm :: Parser Expr
pTerm = pInt <|> (EVar <$> pVar)

pOps :: Parser Expr
pOps = makeExprParser pTerm table
 where
  table = [[ binary "*" (EBin "*")
           , binary "+" (EBin "+")
           , binary "-" (EBin "-")
           ]]
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
