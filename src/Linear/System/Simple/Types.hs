-- |
-- Module:      Linear.System.Simple.Types
-- Description: Types for the Simplex system
-- Copyright:   (c) Junaid Rasheed, 2020-2024
-- License:     BSD-3
-- Maintainer:  jrasheed178@gmail.com
-- Stability:   experimental
module Linear.System.Simple.Types where

import qualified Data.Set as Set
import GHC.Generics (Generic)
import Linear.Constraint.Generic.Types (getGenericConstraintLHS)
import Linear.Constraint.Simple.Types (SimpleConstraint)
import Linear.Constraint.Simple.Util
  ( simpleConstraintVars
  , simplifySimpleConstraint
  )
import Linear.Expr.Util (exprToList)
import Linear.Term.Types (Term (..))
import Linear.Var.Types (Var)

type SimpleSystem = [SimpleConstraint]

simplifySimpleSystem :: SimpleSystem -> SimpleSystem
simplifySimpleSystem = map simplifySimpleConstraint

simpleSystemVars :: SimpleSystem -> Set.Set Var
simpleSystemVars = Set.unions . map simpleConstraintVars

findHighestVar :: SimpleSystem -> Var
findHighestVar simpleSystem =
  let vars =
        [ v | gc <- simpleSystem, term <- exprToList $ getGenericConstraintLHS gc, v <- case term of
                                                                                    VarTerm v -> [v]
                                                                                    CoeffTerm _ v -> [v]
                                                                                    _ -> []
        ]
  in  maximum vars
