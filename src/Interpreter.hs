module Interpreter (
  eval,
  desugar,
) where

import Prelude hiding (read)
import Types

import Control.Monad.Reader (Reader, MonadReader (ask))
import Data.Map.Strict (Map, (!?))
import GHC.Exts (fromList)

type Op = DesugaredExpr -> DesugaredExpr -> DesugaredExpr

testMap :: Map Var Op
testMap = fromList [("+", add)]
 where
  add (DEInt n) (DEInt m) = DEInt $ n + m
  add _         _         = error "add"

eval :: DesugaredExpr -> Reader (Map Var Op) DesugaredExpr
eval = \case
  DEApp f a -> case f of
    DELam param body -> eval $ subst param a body
    e                -> error $ show e <> " is not a function!"
  DEBin op lhs rhs -> ask >>= \m -> case m !? op of
    Just f  -> pure $ f lhs rhs
    Nothing -> error $ "Operation not found: " <> show op
  e -> pure e

subst :: Var -> DesugaredExpr -> DesugaredExpr -> DesugaredExpr
subst var expr = \case
  v@(DEVar v') -> if var == v' then expr else v
  DELam v body ->
    if   var == v
    then subst var expr body
    else DELam v (subst var expr body)
  DEApp f x    -> DEApp (subst var expr f) (subst var expr x)
  DEBin op l r -> DEBin op (subst var expr l) (subst var expr r)
  e            -> e

desugar :: Expr -> DesugaredExpr
desugar = \case
  EInt n        -> DEInt n
  EVar v        -> DEVar v
  EApp ex exs   -> lamHelper exs (desugar ex)
  EBin op l r   -> DEBin op (desugar l) (desugar r)
  ELam [x] body -> DELam x (desugar body)
  ELam xs  body -> DELam (head xs) (desugar $ ELam (tail xs) body)
 where
  lamHelper [x]      l@(DELam _ _   ) = DEApp l (desugar x)
  lamHelper (x : xs) l@(DELam v body) =
    DEApp (DELam v (lamHelper xs body)) (desugar x)
  lamHelper xs e = error $ "Can't apply " <> show xs
                <> " to non-lambda expression " <> show e
