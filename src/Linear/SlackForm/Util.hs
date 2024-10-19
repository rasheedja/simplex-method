-- |
-- Module: Linear.Simplex.StandardForm
-- Description: Standard form of the linear program
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.SlackForm.Util where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Linear.Constraint.Generic.Types
  ( GenericConstraint ((:<=), (:==), (:>=))
  )
import Linear.Constraint.Linear.Types (LinearEquation (..))
import qualified Linear.Constraint.Linear.Util as CLU
import Linear.Constraint.Simple.Types (SimpleConstraint (..))
import Linear.Constraint.Simple.Util
  ( substVarSimpleConstraintExpr
  )
import Linear.Expr.Types (Expr (..), ExprVarsOnly (..))
import Linear.Expr.Util (exprVarsOnlyToExpr)
import Linear.System.Linear.Types (LinearSystem (..))
import qualified Linear.System.Linear.Util as SLU
import Linear.System.Simple.Types
  ( SimpleSystem (..)
  , simplifySimpleSystem
  )
import qualified Linear.System.Simple.Types as SST
import Linear.Term.Types
  ( Term (..)
  , TermVarsOnly (..)
  )
import Linear.Var.Types (Bounds (..), Var, VarBounds)

-- | Eliminate non-zero lower bounds via substitution
-- Return the system with the eliminated variables and a map of the eliminated variables to their equivalent expressions
-- First step here https://en.wikipedia.org/wiki/Simplex_algorithm#Standard_form
eliminateNonZeroLowerBounds ::
  SimpleSystem -> Map.Map Var Expr -> (Map.Map Var Expr, SimpleSystem)
eliminateNonZeroLowerBounds constraints eliminatedVarsMap = aux [] constraints.unSimpleSystem
  where
    -- Eliminate non-zero lower bounds
    aux ::
      [SimpleConstraint] -> [SimpleConstraint] -> (Map.Map Var Expr, SimpleSystem)
    aux _ [] = (eliminatedVarsMap, constraints)
    aux checked (c : cs) = case c of
      -- x >= 5
      (SimpleConstraint (ExprVarsOnly [VarTermVO var] :>= lowerBound)) ->
        if lowerBound == 0
          then aux (checked ++ [c]) cs
          else
            let newVar = SST.nextAvailableVar constraints
                -- y >= 0
                newVarLowerBound = SimpleConstraint $ ExprVarsOnly [VarTermVO newVar] :>= 0

                -- x = y + 5
                substOldVarWith = Expr (VarTerm newVar : [ConstTerm lowerBound])
                substFn = substVarSimpleConstraintExpr var substOldVarWith

                newConstraints =
                  simplifySimpleSystem . SimpleSystem $
                    map substFn checked ++ newVarLowerBound : map substFn cs
                updatedEliminatedVarsMap = Map.insert var substOldVarWith eliminatedVarsMap
            in  eliminateNonZeroLowerBounds newConstraints updatedEliminatedVarsMap -- TODO: Make more efficient if needed
            -- TODO: (do) Deal with == ?
            --       (dont) Or remove == from the type
            --       and convert to <= and >=?
      _ -> aux (checked ++ [c]) cs

-- Add slack variables...
-- Second step here https://en.wikipedia.org/wiki/Simplex_algorithm#Standard_form
-- Return system of equalities and the slack variables
addSlackVariables :: SimpleSystem -> ([Var], LinearSystem)
addSlackVariables constraints =
  let nextAvailableVar = SST.nextAvailableVar constraints
  in  aux constraints.unSimpleSystem nextAvailableVar []
  where
    aux [] _ slackVars = (slackVars, LinearSystem [])
    aux (c : cs) nextVar slackVars = case c of
      (SimpleConstraint (ExprVarsOnly exprTs :<= num)) ->
        let slackVar = nextVar
            newNextVar = nextVar + 1
            newExpr = ExprVarsOnly $ exprTs ++ [VarTermVO slackVar]
            -- slackVarLowerBound = Expr (VarTerm slackVar : []) :>= 0
            (newSlackVars, newConstraints) = aux cs newNextVar slackVars
        in  ( nextVar : newSlackVars
            , SLU.prependLinearEquation (LinearEquation newExpr num) newConstraints
            )
      (SimpleConstraint (ExprVarsOnly exprTs :>= num)) ->
        let slackVar = nextVar
            newNextVar = nextVar + 1
            newExpr = ExprVarsOnly $ exprTs ++ [CoeffTermVO (-1) slackVar]
            -- slackVarLowerBound = Expr (VarTerm slackVar : []) :>= 0
            (newSlackVars, newConstraints) = aux cs newNextVar slackVars
        in  ( nextVar : newSlackVars
            , SLU.prependLinearEquation (LinearEquation newExpr num) newConstraints
            )
      (SimpleConstraint (expr :== num)) ->
        let (newSlackVars, newConstraints) = aux cs nextVar slackVars
        in  ( newSlackVars
            , SLU.prependLinearEquation (LinearEquation expr num) newConstraints
            )

-- Eliminate unrestricted variables (lower bound unknown) given some bounds
-- Third step here https://en.wikipedia.org/wiki/Simplex_algorithm#Standard_form
eliminateUnrestrictedLowerBounds ::
  LinearSystem ->
  VarBounds ->
  Map.Map Var Expr ->
  (Map.Map Var Expr, LinearSystem)
eliminateUnrestrictedLowerBounds constraints varBoundMap eliminatedVarsMap = aux constraints (Map.toList varBoundMap)
  where
    aux ::
      LinearSystem -> [(Var, Bounds)] -> (Map.Map Var Expr, LinearSystem)
    aux _ [] = (eliminatedVarsMap, constraints)
    aux cs ((var, Bounds Nothing _) : bounds) =
      let highestVar = Maybe.fromMaybe (-1) $ SLU.findHighestVar constraints
          newVarPlus = highestVar + 1
          newVarMinus = newVarPlus + 1
          -- newVarPlusLowerBound = Expr (VarTerm newVarPlus : []) :>= 0
          -- newVarMinusLowerBound = Expr (VarTerm newVarMinus : []) :>= 0

          -- oldVar = newVarPlus - newVarMinus
          substOldVarWith = ExprVarsOnly (VarTermVO newVarPlus : [CoeffTermVO (-1) newVarMinus])

          newConstraints =
            LinearSystem $
              map (CLU.substVarWith var substOldVarWith) (unLinearSystem constraints) -- TODO: simplify?
          updatedEliminatedVarsMap = Map.insert var (exprVarsOnlyToExpr substOldVarWith) eliminatedVarsMap
      in  eliminateUnrestrictedLowerBounds
            newConstraints
            (Map.fromList bounds)
            updatedEliminatedVarsMap
    aux cs (_ : bounds) = aux cs bounds
