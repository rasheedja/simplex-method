-- |
-- Module: TestUtil
-- Description: Utility functions for testing
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module TestUtil where

import Control.Monad (forM)
import qualified Data.Map as Map
import Linear.Constraint.Generic.Types
  ( GenericConstraint ((:<=), (:==), (:>=))
  )
import Linear.Constraint.Simple.Types (SimpleConstraint)
import Linear.Constraint.Types
  ( Constraint (..)
  )
import Linear.Expr.Types (Expr)
import Linear.Expr.Util (exprToList)
import Linear.Simplex.Types (VarLitMap)
import Linear.System.Simple.Types (SimpleSystem)
import Linear.Term.Types
  ( Term (..)
  )
import Linear.Var.Types (SimplexNum, Var)
import Test.QuickCheck (Arbitrary (..), Gen)
import Prelude

evalTerm :: VarLitMap -> Linear.Term.Types.Term -> SimplexNum
evalTerm _ (Linear.Term.Types.ConstTerm c) = c
evalTerm varMap (Linear.Term.Types.CoeffTerm c v) =
  c
    * Map.findWithDefault
      (error $ "evalTerm: " <> show v <> " not found in varMap " <> show varMap)
      v
      varMap
evalTerm varMap (Linear.Term.Types.VarTerm v) =
  Map.findWithDefault
    (error $ "evalTerm: " <> show v <> " not found in varMap " <> show varMap)
    v
    varMap

evalExpr :: VarLitMap -> Expr -> SimplexNum
evalExpr varMap expr = sum $ map (evalTerm varMap) $ exprToList expr

evalConstraint :: VarLitMap -> Constraint -> Bool
evalConstraint varMap (lhs :<= rhs) = evalExpr varMap lhs <= evalExpr varMap rhs
evalConstraint varMap (lhs :>= rhs) = evalExpr varMap lhs >= evalExpr varMap rhs
evalConstraint varMap (lhs :== rhs) = evalExpr varMap lhs == evalExpr varMap rhs

evalSimpleConstraint :: VarLitMap -> SimpleConstraint -> Bool
evalSimpleConstraint varMap (lhs :<= rhs) = evalExpr varMap lhs <= rhs
evalSimpleConstraint varMap (lhs :>= rhs) = evalExpr varMap lhs >= rhs
evalSimpleConstraint varMap (lhs :== rhs) = evalExpr varMap lhs == rhs

evalSimpleSystem :: VarLitMap -> SimpleSystem -> Bool
evalSimpleSystem varMap = all (evalSimpleConstraint varMap)

genVarMap :: [Var] -> Gen VarLitMap
genVarMap vars = do
  varVals <- forM vars $ const arbitrary
  pure $ Map.fromList $ zip vars varVals

isConstExpr :: Expr -> Bool
isConstExpr expr =
  let listExpr = exprToList expr
  in  all
        ( \case
            ConstTerm _ -> True
            _ -> False
        )
        listExpr
