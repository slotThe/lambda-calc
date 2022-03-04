{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter (
  eval,
  desugar,
) where

import Types

import Control.Exception
import Data.Foldable
import Prelude hiding (read)


-- | Evaluate a desugared expression within an environment.
eval :: Env -> CheckedExpr -> CheckedExpr
eval env = go
 where
  go :: CheckedExpr -> CheckedExpr
  go = \case
    DEVar v   -> throw $ VariableNotInScope v
    DEApp f x -> case go f of
      -- NOTE: This partial match is safe because we are already type
      -- checked.
      DELam param body -> case go x of
        DEVar v -> throw $ VariableNotInScope v
        y       -> go $ subst param y body
    DEBin op lhs rhs -> case env !? op of
      Just f  -> f (go lhs) (go rhs)
      Nothing -> throw $ OperationNotFound op
    e -> e

-- | Substitute the value of a variable _in the body_ of the lambda that
-- brought it into scope.
--
-- NOTE: This means that, in particular, if our variable is named "a"
-- and we encounter a lambda that also brings a variable named "a" into
-- scope, we immediately exit with the lambda as our final expression
-- and do not substitute any value.  This is essentially a hack to get
-- around having to use De Bruijn indices.
subst :: Var -> CheckedExpr -> CheckedExpr -> CheckedExpr
subst var val = go
 where
  go :: CheckedExpr -> CheckedExpr
  go = \case
    v@(DEVar v')     -> if var == v' then val else v
    l@(DELam v body) -> if var == v  then l   else DELam v (go body)
    DEApp f x        -> DEApp (go f) (go x)
    DEBin op l r     -> DEBin op (go l) (go r)
    e                -> e

-- | Desugar an expression.
desugar :: Expr -> UncheckedExpr
desugar = \case
  EBool b        -> DEBool b
  EInt  n        -> DEInt n
  EStr  s        -> DEStr s
  EVar  v        -> DEVar v
  EApp  f xs     -> foldl' (\g y -> DEApp g (desugar y)) (desugar f) xs
  EBin  op l r   -> DEBin op (desugar l) (desugar r)
  ELam  xs  body -> foldr DELam (desugar body) xs
