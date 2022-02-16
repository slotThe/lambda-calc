module Types (
  Var,
  Expr(..),
  DesugaredExpr(..),
) where

import qualified Data.Text as T
import Data.Text (Text)

type Var = Text

data Expr where
  EInt :: Int -> Expr
  EVar :: Var -> Expr
  ELam :: [Var] -> Expr -> Expr
  EApp :: Expr -> [Expr] -> Expr
  EBin :: Var -> Expr -> Expr -> Expr
  deriving (Show)

data DesugaredExpr where
  DEInt :: Int -> DesugaredExpr
  DEVar :: Var -> DesugaredExpr
  DEBin :: Var -> DesugaredExpr -> DesugaredExpr -> DesugaredExpr
  DELam :: Var -> DesugaredExpr -> DesugaredExpr
  DEApp :: DesugaredExpr -> DesugaredExpr -> DesugaredExpr
  deriving (Show)
