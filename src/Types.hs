module Types (
  -- * The Environment
  Var,
  Env(..),
  (!?),
  builtin,
  -- * Expression Types
  Expr(..),
  DExpr(..),
  -- * Evaluation Errors
  ErrorMsg(..),
) where

import qualified Data.Map.Strict as Map

import Control.Exception
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
import GHC.Exts

type Var = Text
type Op  = DExpr -> DExpr -> DExpr

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

-- | A parsed expression.  This still possibly contains syntax sugar,
-- like (λ a b. …).
data Expr where
  EInt :: Int -> Expr
  EVar :: Var -> Expr
  ELam :: [Var] -> Expr -> Expr
  EApp :: Expr -> [Expr] -> Expr
  EBin :: Var -> Expr -> Expr -> Expr
  deriving (Show)

-- | A desugared expression—all lambdas and applications only take one
-- argument.
data DExpr where
  DEInt :: Int -> DExpr
  DEVar :: Var -> DExpr
  DEBin :: Var -> DExpr -> DExpr -> DExpr
  DELam :: Var -> DExpr -> DExpr
  DEApp :: DExpr -> DExpr -> DExpr

instance Show DExpr where
  show :: DExpr -> String
  show = \case
    DEInt n      -> show n
    DEVar v      -> unpack v
    DEBin op l r -> show l <> " " <> unpack op <> " " <> show r
    DELam v x    -> "λ" <> unpack v <> ". " <> show x
    DEApp f x    -> "(" <> show f <> ") " <> show x

data ErrorMsg
  = VariableNotInScope Var
  | OperationNotFound  Var
  | NotAFunction DExpr
  | NoLambdaApplication [Expr] DExpr

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
