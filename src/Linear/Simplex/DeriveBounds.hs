module Linear.Simplex.DeriveBounds where

import Prelude hiding (EQ)

import Control.Applicative (liftA2)
import Control.Lens hiding (Const)
import Data.Generics.Labels ()
import Data.List (sort)
import GHC.Generics (Generic)

import Linear.Simplex.Types

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromMaybe)

-- | Update the bounds for a variable in the map via intersection
updateBounds :: Var -> Bounds -> Map Var Bounds -> Map Var Bounds
updateBounds var newBounds boundsMap =
  let existingBounds = Map.findWithDefault (Bounds Nothing Nothing) var boundsMap
      updatedBounds = combineBounds newBounds existingBounds
  in Map.insert var updatedBounds boundsMap
  
-- Intersection of two bounds
combineBounds :: Bounds -> Bounds -> Bounds
combineBounds newBounds existingBounds = do
  let newLowerBound =
        case (newBounds.lowerBound, existingBounds.lowerBound) of
          (Just newLowerBound, Just existingLowerBound) -> Just $ max newLowerBound existingLowerBound
          (_, Just existingLowerBound) -> Just existingLowerBound
          (Just newLowerBound, _) -> Just newLowerBound
          (_, _) -> Nothing
  let newUpperBound =
        case (newBounds.upperBound, existingBounds.upperBound) of
          (Just newUpperBound, Just existingUpperBound) -> Just $ max newUpperBound existingUpperBound
          (_, Just existingUpperBound) -> Just existingUpperBound
          (Just newUpperBound, _) -> Just newUpperBound
          (_, _) -> Nothing
  Bounds newLowerBound newUpperBound

-- Helper to recursively analyze expressions and derive bounds
-- deriveBoundsFromExpr :: Expr -> Bounds -> (Var -> Bounds -> Map Var Bounds -> Map Var Bounds)
-- deriveBoundsFromExpr (Var v) bounds accMap = updateBounds v bounds accMap
-- deriveBoundsFromExpr (Const c) _ accMap = accMap
-- deriveBoundsFromExpr (e1 :+ e2) bounds accMap =
--   deriveBoundsFromExpr e1 bounds $ deriveBoundsFromExpr e2 bounds accMap
-- deriveBoundsFromExpr (e1 :-: e2) bounds accMap =
--   deriveBoundsFromExpr e1 bounds $ deriveBoundsFromExpr e2 bounds accMap
-- deriveBoundsFromExpr (e1 :*: e2) bounds accMap =
--   deriveBoundsFromExpr e1 bounds $ deriveBoundsFromExpr e2 bounds accMap
-- deriveBoundsFromExpr (e1 :/: e2) bounds accMap =
--   deriveBoundsFromExpr e1 bounds $ deriveBoundsFromExpr e2 bounds accMap

-- Function to derive bounds from a single constraint for a variable
deriveVarBoundsFromConstraint :: Constraint -> Map Var Bounds -> Map Var Bounds
-- deriveVarBoundsFromConstraint (Var v :<=: Const u) accMap = updateBounds v (Bounds Nothing (Just u)) accMap
-- deriveVarBoundsFromConstraint (Var v :>=: Const l) accMap = updateBounds v (Bounds (Just l) Nothing) accMap
-- deriveVarBoundsFromConstraint (Var v :==: Const c) accMap = updateBounds v (Bounds (Just c) (Just c)) accMap
deriveVarBoundsFromConstraint _ accMap = accMap  -- Ignore non-constant expressions

-- Function to derive bounds for all variables in the constraints list
deriveBounds :: [Constraint] -> Map Var Bounds
deriveBounds constraints = foldr deriveVarBoundsFromConstraint Map.empty constraints

-- data Bounds = Bounds
-- { lowerBound :: Maybe SimplexNum
-- , upperBound :: Maybe SimplexNum
-- }
-- deriving (Show, Read, Eq, Generic)

validateBounds :: Map Var Bounds -> Bool
validateBounds boundsMap = all soundBounds $ Map.toList boundsMap
  where
    soundBounds (_, Bounds lowerBound upperBound) =
      case (lowerBound, upperBound) of
        (Just l, Just u) -> l <= u
        (_, _) -> True


type AuxVarMap = Map.Map Var (Var, Var)

splitNegativeVars :: Map Var Bounds -> (Map Var Bounds, AuxVarMap)
splitNegativeVars boundsMap = 
  let (newBoundsMap, auxMap, _) = Map.foldrWithKey splitVar (Map.empty, Map.empty, Map.size boundsMap + 1) boundsMap
  in (newBoundsMap, auxMap)
  where
    splitVar var (Bounds lowerBound upperBound) (newBoundsMap, auxMap, nextVar) =
      if fromMaybe (-1) lowerBound < 0
        then let var1 = nextVar
                 var2 = nextVar + 1
                 newBounds = Bounds (Just 0) Nothing
             in (Map.insert var1 newBounds $ Map.insert var2 newBounds newBoundsMap,
                 Map.insert var (var1, var2) auxMap,
                 nextVar + 2)
        else (Map.insert var (Bounds lowerBound upperBound) newBoundsMap, auxMap, nextVar)

-- PLAN:

-- Accept systems with any kind of constraints (<=, >=, ==)
-- Identify all variables in the system and their bounds
-- For any variable with negative/unbounded lower bound, split it into two variables with lower bound 0
-- So, say x has lower bound -1, then we split it into x1 and x2, where x1 >= 0 and x2 >= 0
-- Then substitute x with x1 - x2 in all constraints
-- All variables now have non-negative lower bounds
-- Now, we proceed with the remaining transformations
-- Slack variables are introduced for all constraints
-- Artificial variables are introduced for all constraints with equality
-- and so on until we have a system in the standard form

-- Maybe have a type for the standard system? It would be a list of linear equalities with a constant on the RHS
-- All variables >= 0 can be assumed, doesn't need to be in the type
-- The objective function can be considered separate, so not part of the standard system type?