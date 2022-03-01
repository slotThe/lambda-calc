module Types (
  -- * The Environment
  Var,
  Env(..),
  (!?),
  builtin,
  -- * Expression Types
  Expr(..),
  DExpr(..),
  -- * The Type System
  Type(..),
  Infer,
  TVar(..),
  Constraints,
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
  deriving stock (Eq, Show)

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

------------------------------------------------------------------------
-- Our Type System

newtype TVar = TVar Int
  deriving newtype (Show, Eq, Ord)

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

-- | The monad in which type inference will take place.
type Infer a = State [TVar] a

------------------------------------------------------------------------
-- Error Handling

data ErrorMsg where
  VariableNotInScope  :: Var -> ErrorMsg
  OperationNotFound   :: Var -> ErrorMsg
  NotAFunction        :: DExpr -> ErrorMsg
  NoLambdaApplication :: [Expr] -> DExpr -> ErrorMsg
  UnificationError    :: Type -> Type -> ErrorMsg
  OccursError         :: Type -> Type -> ErrorMsg

instance Exception ErrorMsg

instance Show ErrorMsg where
  show :: ErrorMsg -> String
  show = \case
    VariableNotInScope v -> "Variable not in scope: " <> show v <> "."
    OperationNotFound op -> "Operation not found: " <> show op <> "."
    NotAFunction expr    ->
      "Can't apply " <> show expr <> " because it is not a function."
    NoLambdaApplication app notLam ->
      "Can't apply " <> show app <> " to non-lambda expression " <> show notLam <> "."
    UnificationError t1 t2 ->
      "Can't match type `" <> show t1 <> "' with type `" <> show t2 <> "'."
    OccursError t1 t2 ->
      "Can't construct infinite type: `" <> show t1 <> "' ~ `" <> show t2 <> "'."
