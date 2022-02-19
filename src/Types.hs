module Types (
  -- * The Environment
  Var,
  Env(..),
  (!?),
  builtin,
  -- * Expression Types
  Expr(..),
  DesugaredExpr(..),
  -- * Evaluation Errors
  ErrorMsg(..),
) where

import qualified Data.Map.Strict as Map

import Control.Exception
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
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

instance Show DesugaredExpr where
  show :: DesugaredExpr -> String
  show = \case
    DEInt n      -> show n
    DEVar v      -> unpack v
    DEBin op l r -> show l <> " " <> unpack op <> " " <> show r
    DELam v x    -> "λ" <> unpack v <> ". " <> show x
    DEApp f x    -> "(" <> show f <> ") " <> show x

data ErrorMsg
  = VariableNotInScope Var
  | OperationNotFound  Var
  | NotAFunction DesugaredExpr
  | NoLambdaApplication [Expr] DesugaredExpr

instance Exception ErrorMsg

instance Show ErrorMsg where
  show :: ErrorMsg -> String
  show = \case
    VariableNotInScope v -> "Variable not in scope: " <> show v
    OperationNotFound op -> "Operation not found: " <> show op
    NotAFunction expr    ->
      "Can't apply " <> show expr <> " because it is not a function."
    NoLambdaApplication app notLam ->
      "Can't apply " <> show app <> " to non-lambda expression " <> show notLam
