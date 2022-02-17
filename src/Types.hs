module Types (
  -- * The Environment
  Var,
  Env(..),
  (!?),
  builtin,
  -- * Expression Types
  Expr(..),
  DesugaredExpr(..),
) where

import qualified Data.Map.Strict as Map

import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Exts

type Var = Text
type Op  = DesugaredExpr -> DesugaredExpr -> DesugaredExpr

newtype Env = Env (Map Var Op)
  deriving newtype (IsList)

(!?) :: Env -> Var -> Maybe Op
(!?) = coerce ((Map.!?) @Var @Op)

builtin :: Env
builtin = fromList
  [ ("+", add)
  , ("-", sub)
  , ("*", mul)
  ]
 where
  add (DEInt n) (DEInt m) = DEInt $ n + m
  add _         _         = error "add"

  sub (DEInt n) (DEInt m) = DEInt $ n - m
  sub _         _         = error "sub"

  mul (DEInt n) (DEInt m) = DEInt $ n * m
  mul _         _         = error "mul"

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
