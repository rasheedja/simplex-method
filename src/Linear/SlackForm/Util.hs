-- |
-- Module: Linear.Simplex.StandardForm
-- Description: Standard form of the linear program
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.SlackForm.Util where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Linear.Constraint.Generic.Types
  ( GenericConstraint ((:<=), (:==), (:>=))
  )
import Linear.Constraint.Simple.Util
  ( substVarSimpleConstraint
  )
import Linear.Expr.Types (Expr (..))
import Linear.System.Simple.Types
  ( SimpleSystem
  , findHighestVar
  , simplifySimpleSystem
  )
import Linear.Term.Types
  ( Term (..)
  )
import Linear.Var.Types (Bounds (..), Var, VarBounds)

-- | Eliminate non-zero lower bounds via substitution
-- Return the system with the eliminated variables and a map of the eliminated variables to their equivalent expressions
-- First step here https://en.wikipedia.org/wiki/Simplex_algorithm#Standard_form
eliminateNonZeroLowerBounds ::
  SimpleSystem -> Map.Map Var Expr -> (Map.Map Var Expr, SimpleSystem)
eliminateNonZeroLowerBounds constraints eliminatedVarsMap = aux [] constraints
  where
    -- Eliminate non-zero lower bounds
    aux _ [] = (eliminatedVarsMap, constraints)
    aux checked (c : cs) = case c of
      -- x >= 5
      (Expr (VarTerm var :| []) :>= lowerBound) ->
        if lowerBound == 0
          then aux (checked ++ [c]) cs
          else
            let newVar = findHighestVar constraints + 1
                -- y >= 0
                newVarLowerBound = Expr (VarTerm newVar :| []) :>= 0

                -- x = y + 5
                substOldVarWith = Expr (VarTerm newVar :| [ConstTerm lowerBound])
                substFn = substVarSimpleConstraint var substOldVarWith

                newConstraints =
                  simplifySimpleSystem $ map substFn checked ++ newVarLowerBound : map substFn cs
                updatedEliminatedVarsMap = Map.insert var substOldVarWith eliminatedVarsMap
            in  eliminateNonZeroLowerBounds newConstraints updatedEliminatedVarsMap -- TODO: Make more efficient if needed
            -- TODO: (do) Deal with == ?
            --       (dont) Or remove == from the type
            --       and convert to <= and >=?
      _ -> aux (checked ++ [c]) cs

-- Add slack variables...
-- Second step here https://en.wikipedia.org/wiki/Simplex_algorithm#Standard_form
-- Return system of equalities and the slack variables
addSlackVariables :: SimpleSystem -> ([Var], SimpleSystem)
addSlackVariables constraints =
  let nextAvailableVar = findHighestVar constraints + 1
  in  aux constraints nextAvailableVar []
  where
    aux :: SimpleSystem -> Var -> [Var] -> ([Var], SimpleSystem)
    aux [] _ slackVars = (slackVars, [])
    aux (c : cs) nextVar slackVars = case c of
      (expr@(Expr exprTs) :<= num) ->
        let slackVar = nextVar
            newNextVar = nextVar + 1
            newExpr = Expr $ NE.appendList exprTs [VarTerm slackVar]
            slackVarLowerBound = Expr (VarTerm slackVar :| []) :>= 0
            (newSlackVars, newConstraints) = aux cs newNextVar slackVars
        in  (nextVar : newSlackVars, newExpr :== num : slackVarLowerBound : newConstraints)
      (expr@(Expr exprTs) :>= num) ->
        let slackVar = nextVar
            newNextVar = nextVar + 1
            newExpr = Expr $ NE.appendList exprTs [CoeffTerm (-1) slackVar]
            slackVarLowerBound = Expr (VarTerm slackVar :| []) :>= 0
            (newSlackVars, newConstraints) = aux cs newNextVar slackVars
        in  (nextVar : newSlackVars, newExpr :== num : slackVarLowerBound : newConstraints)
      (expr :== num) ->
        let (newSlackVars, newConstraints) = aux cs nextVar slackVars
        in  (newSlackVars, c : newConstraints)

-- Eliminate unrestricted variables (lower bound unknown)
-- Third step here https://en.wikipedia.org/wiki/Simplex_algorithm#Standard_form
-- precondition: VarBounds accurate for SimpleSystem
eliminateUnrestrictedLowerBounds ::
  SimpleSystem ->
  VarBounds ->
  Map.Map Var Expr ->
  (Map.Map Var Expr, SimpleSystem)
eliminateUnrestrictedLowerBounds constraints varBoundMap eliminatedVarsMap = aux constraints (Map.toList varBoundMap)
  where
    aux :: SimpleSystem -> [(Var, Bounds)] -> (Map.Map Var Expr, SimpleSystem)
    aux _ [] = (eliminatedVarsMap, constraints)
    aux cs ((var, Bounds Nothing _) : bounds) =
      let newVarPlus = findHighestVar constraints + 1
          newVarMinus = newVarPlus + 1
          newVarPlusLowerBound = Expr (VarTerm newVarPlus :| []) :>= 0
          newVarMinusLowerBound = Expr (VarTerm newVarMinus :| []) :>= 0

          -- oldVar = newVarPlus - newVarMinus
          substOldVarWith = Expr (VarTerm newVarPlus :| [CoeffTerm (-1) newVarMinus])

          newConstraints =
            simplifySimpleSystem $
              newVarPlusLowerBound
                : newVarMinusLowerBound
                : map (substVarSimpleConstraint var substOldVarWith) constraints
          -- TODO: Update this name
          updatedEliminatedVarsMap = Map.insert var substOldVarWith eliminatedVarsMap
      in  eliminateUnrestrictedLowerBounds
            newConstraints
            (Map.fromList bounds)
            updatedEliminatedVarsMap
    aux cs (_ : bounds) = aux cs bounds
