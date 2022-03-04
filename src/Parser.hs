module Parser (read) where

import Types

import Data.Text                  qualified as T
import Text.Megaparsec            qualified as P
import Text.Megaparsec.Char       qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

import Data.Char
import Data.Functor
import Data.Text (Text)
import Data.Void (Void)
import Prelude hiding (read)
import Text.Megaparsec (empty, (<?>), (<|>))


type Parser = P.Parsec Void Text

read :: Text -> Either String Expr
read inp = case P.parse ((P.try pOps <|> pLambda) <* P.eof) "" inp of
  Left err   -> Left $ P.errorBundlePretty err
  Right expr -> Right expr

pCon :: Parser Expr
pCon = pInt <|> pStr <|> pBool
 where
  pInt, pStr, pBool :: Parser Expr
  pInt  = EInt  <$> L.signed mempty L.decimal        <?> "integer"
  pStr  = EStr  <$> ("\"" *> P.takeWhileP Nothing (/= '"')  <* "\"") <?> "string"
  pBool = EBool <$> ("#t" $> True <|> "#f" $> False) <?> "boolean"

pVar :: Parser Var
pVar = lexeme (T.cons <$> P.letterChar <*> takeSymbol) <?> "variable"

pLambda :: Parser Expr
pLambda = optParens $ space *> (<?> "lambda") do
  _  <- symbol "\\" <|> symbol "λ"
  vs <- pVar `P.sepBy` space
  _  <- symbol "." <|> symbol "->" <|> symbol "→"
  e  <- lexeme $ P.try pOps <|> pLambda
  pure $ ELam vs e

pApp :: Parser Expr
pApp = optParens $ (<?> "application") do
  f <- (EVar <$> pVar) <|> ("(" *> pLambda <* ")") <?> "lambda or symbol"
  apps <- space *> (pLambda <|> P.try pTerm') `P.sepBy1` space  -- prefer lambda
  pure $ EApp f apps

pTerm :: Parser Expr
pTerm = pCon
    <|> P.try pApp
    <|> EVar <$> pVar
    <|> P.try ("(" *> pOps <* ")")

pTerm' :: Parser Expr
pTerm' = pCon
     <|> EVar <$> pVar
     <|> P.try pApp
     <|> P.try ("(" *> pOps <* ")")

pOps :: Parser Expr
pOps = pTerm
  `chainl1` (EBin "*" <$ symbol "*")
  `chainl1` P.choice [ EBin "++" <$ symbol "++"
                     , EBin "+"  <$ symbol "+"
                     , EBin "-"  <$ symbol "-"
                     ]

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

-- | Apply the parser @p@ at least once; apply the results of the left
-- associative parser @op@ to results of @p@.
chainl1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
 where
  rest :: a -> Parser a
  rest x = do f <- op
              y <- p
              rest (f x y)
       <|> pure x
