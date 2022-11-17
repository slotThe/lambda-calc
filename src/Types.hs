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
  LcType(..),
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
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
import GHC.Exts (IsList(fromList), coerce)

type Var :: Type
type Var = Text

type Op :: Type
type Op  = CheckedExpr -> CheckedExpr -> CheckedExpr

type Env :: Type
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
type Expr :: Type
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
type DExpr :: Type -> Type
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

type Checked :: Type
data Checked

type CheckedExpr :: Type
type CheckedExpr = DExpr Checked

type Unchecked :: Type
data Unchecked

type UncheckedExpr :: Type
type UncheckedExpr = DExpr Unchecked

-- | A type variable.
type TVar :: Type
newtype TVar = TVar Int
  deriving newtype (Show, Eq, Ord)

-- | The monad in which type inference will take place.
type Infer :: Type -> Type
type Infer a = State [TVar] a

-- | The type... of a type!
type LcType :: Type
data LcType where
  TyVar :: TVar -> LcType             -- ^ A type variable.
  TyCon :: String -> LcType           -- ^ A (built-in) type constructor, like @Int@.
  (:->) :: LcType -> LcType -> LcType -- ^ Type Arrow—and a happy one at that!
  deriving stock (Eq, Ord)

infixr 5 :->

instance Show LcType where
  show :: LcType -> String
  show = \case
    TyVar (TVar tv) -> if tv <= 26 then [chr (tv + 96)] else "a" <> show tv
    TyCon tc        -> tc
    -- Properly show higher-order functions.
    arr@(_ :-> _) :-> ty  -> "(" <> show arr <> ") → " <> show ty
    ty            :-> ty' -> show ty <> " → " <> show ty'

-- | A type constraint of the form @ty ~ ty'@.
type Constraints :: Type
type Constraints = [(LcType, LcType)]

-- built-in types

pattern TyStr, TyBool, TyInt :: LcType
pattern TyStr  = TyCon "String"
pattern TyInt  = TyCon "Int"
pattern TyBool = TyCon "Bool"

------------------------------------------------------------------------
-- Error Handling

type ErrorMsg :: Type
data ErrorMsg where
  VariableNotInScope :: Var -> ErrorMsg
  OperationNotFound  :: Var -> ErrorMsg
  UnificationError   :: LcType -> LcType -> ErrorMsg
  OccursError        :: LcType -> LcType -> ErrorMsg

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
