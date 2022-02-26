module Interpreter (
  eval,
  desugar,
) where

import Prelude hiding (read)
import Control.Exception
import Types


-- | Evaluate a desugared expression within an environment.
eval :: Env -> DExpr -> DExpr
eval env = go
 where
  go :: DExpr -> DExpr
  go = \case
    DEVar v   -> throw $ VariableNotInScope v
    DEApp f x -> case go f of
      DELam param body -> case go x of
        DEVar v -> throw $ VariableNotInScope v
        y       -> go $ subst param y body
      e -> throw $ NotAFunction e
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
subst :: Var -> DExpr -> DExpr -> DExpr
subst var val = go
 where
  go :: DExpr -> DExpr
  go = \case
    v@(DEVar v')     -> if var == v' then val else v
    l@(DELam v body) -> if var == v  then l   else DELam v (go body)
    DEApp f x        -> DEApp (go f) (go x)
    DEBin op l r     -> DEBin op (go l) (go r)
    e                -> e

-- | Desugar an expression.
desugar :: Expr -> DExpr
desugar = \case
  EInt n        -> DEInt n
  EVar v        -> DEVar v
  EApp f xs     -> desugarLambs xs (desugar f)
  EBin op l r   -> DEBin op (desugar l) (desugar r)
  ELam [x] body -> DELam x (desugar body)
  ELam xs  body -> DELam (head xs) (desugar $ ELam (tail xs) body)
 where
  desugarLambs [x]      lamb@DELam{}   = DEApp lamb (desugar x)
  desugarLambs (x : xs) (DELam v body) =
    DEApp (DELam v (desugarLambs xs body)) (desugar x)
  desugarLambs xs e = throw $ NoLambdaApplication xs e
