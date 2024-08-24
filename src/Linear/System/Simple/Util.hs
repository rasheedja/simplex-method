-- |
-- Module:      Linear.System.Simple.Util
-- Description: Utility functions for the Simplex method
-- Copyright:   (c) Junaid Rasheed, 2020-2024
-- License:     BSD3
-- Maintainer:  jrasheed178@gmail.com
-- Stability:   experimental
module Linear.System.Simple.Util where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as Set
import Linear.Constraint.Generic.Types
  ( GenericConstraint ((:<=), (:==), (:>=))
  )
import Linear.Constraint.Simple.Types (SimpleConstraint)
import Linear.Expr.Types (Expr (..))
import Linear.System.Simple.Types
  ( SimpleSystem
  , simpleSystemVars
  )
import Linear.Term.Types (Term (..))
import Linear.Var.Types (Bounds (..), VarBounds)

-- | Derive bounds for all variables in a system
deriveBounds :: SimpleSystem -> VarBounds
deriveBounds simpleSystem = foldr updateBounds initialVarBounds simpleSystem
  where
    systemVars = simpleSystemVars simpleSystem
    initialVarBounds = M.fromList [(v, Bounds Nothing Nothing) | v <- Set.toList systemVars]

    updateBounds :: SimpleConstraint -> VarBounds -> VarBounds
    updateBounds (Expr ((VarTerm var) :| []) :<= num) = M.insertWith mergeBounds var (Bounds Nothing (Just num))
    updateBounds (Expr ((VarTerm var) :| []) :>= num) = M.insertWith mergeBounds var (Bounds (Just num) Nothing)
    updateBounds (Expr ((VarTerm var) :| []) :== num) = M.insertWith mergeBounds var (Bounds (Just num) (Just num))
    updateBounds _ = id

    -- \| Merge two bounds, very simple
    mergeBounds :: Bounds -> Bounds -> Bounds
    mergeBounds (Bounds l1 u1) (Bounds l2 u2) = Bounds (mergeLower l1 l2) (mergeUpper u1 u2)
      where
        mergeLower Nothing b = b
        mergeLower a Nothing = a
        mergeLower (Just a) (Just b) = Just (max a b)

        mergeUpper Nothing b = b
        mergeUpper a Nothing = a
        mergeUpper (Just a) (Just b) = Just (min a b)

-- Eliminate inequalities which are outside the bounds
-- precondition: no zero coefficients
-- TODO: better name
removeUselessSystemBounds :: SimpleSystem -> VarBounds -> SimpleSystem
removeUselessSystemBounds constraints bounds =
  filter
    ( \case
        (Expr ((VarTerm var) :| []) :<= num) -> case M.lookup var bounds of
          Just (Bounds _ (Just upper)) -> num <= upper
          _ -> True
        (Expr ((VarTerm var) :| []) :>= num) -> case M.lookup var bounds of
          Just (Bounds (Just lower) _) -> num >= lower
          _ -> True
        _ -> True
    )
    constraints
