module Checker (
  check,
  checkExpr,
) where

import Types

import Data.Map.Strict qualified as Map
import Data.Set        qualified as Set

import Control.Exception
import Control.Monad.State
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Functor
import Data.Map (Map)
import Data.Set (Set)
import GHC.Exts (fromList, toList)


-- | Type check an unchecked expression and return the checked
-- expression if possible.
checkExpr :: UncheckedExpr -> CheckedExpr
checkExpr expr = let !_ = check expr in coerce expr

-- | Type check an unchecked expression.
check :: UncheckedExpr -> Type
check expr = normalise $ refine (evalState (unify constraints) []) ty
 where
  (ty, constraints) = evalState (infer context expr) (TVar <$> [1..])

  context :: Map Var Type
  context = fromList
    [ ("+" , TyInt :-> TyInt :-> TyInt)
    , ("-" , TyInt :-> TyInt :-> TyInt)
    , ("*" , TyInt :-> TyInt :-> TyInt)
    , ("++", TyStr :-> TyStr :-> TyStr)
    ]

-- | Normalise a type; i.e., normalise the list of type variables from,
-- for example, @[4, 6, 15]@ to @[1, 2, 3]@.
normalise :: Type -> Type
normalise ty = go ty
 where
  go :: Type -> Type
  go = \case
    TyVar tv -> TyVar (vars Map.! tv)
    l :-> r  -> go l :-> go r
    t        -> t

  vars :: Map TVar TVar
  vars = fromList $ toList (allVars ty) `zip` map TVar [1 ..]
   where
    allVars :: Type -> Set TVar
    allVars = \case
      TyVar tv -> Set.singleton tv
      l :-> r  -> allVars l <> allVars r
      TyCon{}  -> mempty

-- | Given some type context, infer a type and generate constraints for
-- the given expression.
infer :: Map Var Type -> UncheckedExpr -> Infer (Type, Constraints)
infer context = \case
  DEBool{} -> pure (TyBool, mempty)
  DEInt{}  -> pure (TyInt, mempty)
  DEStr{}  -> pure (TyStr, mempty)
  DEVar v  -> case context Map.!? v of
    Nothing -> throw $ VariableNotInScope v
    Just v' -> pure (v', mempty)
  DELam v body -> do
    startTy      <- TyVar <$> freshTVar
    (retTy, con) <- infer (Map.insert v startTy context) body
    pure (startTy :-> retTy, con)
  DEApp f x    -> do
    (fTy, fCon) <- infer context f
    (xTy, xCon) <- infer context x
    retTy <- TyVar <$> freshTVar
    pure (retTy, (fTy, xTy :-> retTy) : (fCon <> xCon))
  DEBin op l r -> do
    (opTy, opCon) <- infer context (DEVar op)  -- op must be in context
    (lTy , lCon)  <- infer context l
    (rTy , rCon)  <- infer context r
    retTy <- TyVar <$> freshTVar
    pure (retTy, (opTy, lTy :-> rTy :-> retTy) : (opCon <> lCon <> rCon))

-- | Unify constraints.
unify :: Constraints -> Infer (Map TVar Type)
unify = \case
  []           -> pure mempty
  (c, c') : cs -> if c == c' then unify cs else go c c'
   where
    go :: Type -> Type -> Infer (Map TVar Type)
    go c1@(TyVar v) c2           = refineVar v c1 c2
    go c1           c2@(TyVar v) = refineVar v c2 c1
    go (l :-> r)    (l' :-> r')  = unify $ [(l, l'), (r, r')] <> cs
    go t1           t2           = throw $ UnificationError t1 t2

    -- | Imbue a type variable with more information.
    refineVar :: TVar -> Type -> Type -> Infer (Map TVar Type)
    refineVar tyVar tyA tyB =
      if tyVar `occursIn` tyB
      then throw $ OccursError tyA tyB
      -- Refine constraints and unify these ones.
      else unify (bimap (refine bTyMap) (refine bTyMap) <$> cs)
           -- There may have been a better refinement found already, so
           -- exploit the left-biasedness of @(<>)@.
           <&> (<> bTyMap)
     where bTyMap = Map.singleton tyVar tyB

    occursIn :: TVar -> Type -> Bool
    v `occursIn` TyVar y   = v == y
    v `occursIn` (a :-> b) = v `occursIn` a || v `occursIn` b
    _ `occursIn` TyCon{}   = False

-- | Refine a type based on a map of possible refinements.
--
-- The idea is that the map may have additional information for the type
-- that we don't yet know about.  E.g., it may contain the key-value
-- pair @(TVar 2, TyCon "Int")@ which, when calling refine on the type
-- @TyVar (TVar 2)@ would return @TyCon "Int"@.
refine :: Map TVar Type -> Type -> Type
refine tyMap t = Map.foldrWithKey' go t tyMap
 where
  go :: TVar -> Type -> Type -> Type
  go tvar ty = \case
    TyVar tv   -> if tvar == tv then ty else TyVar tv
    x :-> z    -> go tvar ty x :-> go tvar ty z
    tc@TyCon{} -> tc

-- | Generate a fresh type variable that's never been used before.
freshTVar :: Infer TVar
freshTVar = get >>= \case
  (x : xs) -> put xs $> x
  []       -> error "I ran out of type variables!  Send help!"
