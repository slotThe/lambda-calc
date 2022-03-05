{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Types (
  -- * The Environment
  Var,
  Env(..), (!?),
  builtin,
  -- * Expression Types
  Expr(..),
  DExpr(..), CheckedExpr, UncheckedExpr,
  -- * The Type System
  Type(..),
  Infer,
  TVar(..),
  Constraints,
  pattern TyStr, pattern TyInt, pattern TyBool,
  -- * Evaluation Errors
  ErrorMsg(..),
) where

import Data.Map.Strict qualified as Map

import Control.Exception
import Control.Monad.State
import Data.Char
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
import GHC.Exts (IsList(fromList), coerce)

type Var = Text
type Op  = CheckedExpr -> CheckedExpr -> CheckedExpr

newtype Env = Env (Map Var Op)
  deriving newtype (IsList)

(!?) :: Env -> Var -> Maybe Op
(!?) = coerce ((Map.!?) @Var @Op)

builtin :: Env
builtin = fromList
  [ ("+" , add)
  , ("-" , sub)
  , ("*" , mul)
  , ("++", cat)
  ]
 where
  -- These are safe because we are handling type-checked expressions.
  add, sub, mul, cat :: CheckedExpr -> CheckedExpr -> CheckedExpr
  add (DEInt n) (DEInt m) = DEInt $ n + m
  sub (DEInt n) (DEInt m) = DEInt $ n - m
  mul (DEInt n) (DEInt m) = DEInt $ n * m
  cat (DEStr s) (DEStr t) = DEStr $ s <> t

-- | A parsed expression.  This still possibly contains syntax sugar,
-- like (λ a b. …).
data Expr where
  EBool :: Bool -> Expr
  EStr  :: Text -> Expr
  EInt  :: Int -> Expr
  EVar  :: Var -> Expr
  ELam  :: [Var] -> Expr -> Expr
  EApp  :: Expr -> [Expr] -> Expr
  EBin  :: Var -> Expr -> Expr -> Expr
  deriving stock (Eq, Show)

-- | A desugared expression—all lambdas and applications only take one
-- argument.
data DExpr a where
  DEBool :: Bool -> DExpr a
  DEStr  :: Text -> DExpr a
  DEInt  :: Int -> DExpr a
  DEVar  :: Var -> DExpr a
  DEBin  :: Var -> DExpr a -> DExpr a -> DExpr a
  DELam  :: Var -> DExpr a -> DExpr a
  DEApp  :: DExpr a -> DExpr a -> DExpr a

instance Show (DExpr a) where
  show :: DExpr a -> String
  show = \case
    DEBool b     -> show b
    DEStr  n      -> unpack n
    DEInt  n      -> show n
    DEVar  v      -> unpack v
    DEBin  op l r -> show l <> " " <> unpack op <> " " <> show r
    DELam  v x    -> "λ" <> unpack v <> ". " <> show x
    DEApp  f x    -> "(" <> show f <> ") " <> show x

------------------------------------------------------------------------
-- Our Type System

-- Phantom types to differentiate checked and unchecked expressions.

data Checked
type CheckedExpr   = DExpr Checked

data Unchecked
type UncheckedExpr = DExpr Unchecked

-- | A type variable.
newtype TVar = TVar Int
  deriving newtype (Show, Eq, Ord)

-- | The monad in which type inference will take place.
type Infer a = State [TVar] a

-- | The type... of a type!
data Type where
  TyVar :: TVar -> Type         -- ^ A type variable.
  TyCon :: String -> Type       -- ^ A (built-in) type constructor, like @Int@.
  (:->) :: Type -> Type -> Type -- ^ Type Arrow—and a happy one at that!
  deriving stock (Eq, Ord)

infixr 5 :->

instance Show Type where
  show :: Type -> String
  show = \case
    TyVar (TVar tv) -> if tv <= 26 then [chr (tv + 96)] else "a" <> show tv
    TyCon tc        -> tc
    -- Properly show higher-order functions.
    arr@(_ :-> _) :-> ty  -> "(" <> show arr <> ") → " <> show ty
    ty            :-> ty' -> show ty <> " → " <> show ty'

-- | A type constraint of the form @ty ~ ty'@.
type Constraints = [(Type, Type)]

-- built-in types

pattern TyStr, TyBool, TyInt :: Type
pattern TyStr  = TyCon "String"
pattern TyInt  = TyCon "Int"
pattern TyBool = TyCon "Bool"

------------------------------------------------------------------------
-- Error Handling

data ErrorMsg where
  VariableNotInScope :: Var -> ErrorMsg
  OperationNotFound  :: Var -> ErrorMsg
  UnificationError   :: Type -> Type -> ErrorMsg
  OccursError        :: Type -> Type -> ErrorMsg

instance Exception ErrorMsg

instance Show ErrorMsg where
  show :: ErrorMsg -> String
  show = \case
    VariableNotInScope v   -> "Variable not in scope: " <> show v <> "."
    OperationNotFound op   -> "Operation not found: " <> show op <> "."
    UnificationError t1 t2 ->
      "Can't match type `" <> show t1 <> "' with type `" <> show t2 <> "'."
    OccursError t1 t2      ->
      "Can't construct infinite type: `" <> show t1 <> "' ~ `" <> show t2 <> "'."
