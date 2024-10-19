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
import Linear.Expr.Util (exprVarsOnlyToList)
import Linear.System.Types (System)
import Linear.Term.Types (TermVarsOnly (..))
import Linear.Var.Types (Var)
import Test.QuickCheck (Arbitrary (..))

-- TODO: Use a more descriptive name
newtype SimpleSystem = SimpleSystem {unSimpleSystem :: [SimpleConstraint]}
  deriving (Show, Eq, Read, Generic)

instance Arbitrary SimpleSystem where
  arbitrary = SimpleSystem <$> arbitrary

simplifySimpleSystem :: SimpleSystem -> SimpleSystem
simplifySimpleSystem = SimpleSystem . map simplifySimpleConstraint . unSimpleSystem

simpleSystemVars :: SimpleSystem -> Set.Set Var
simpleSystemVars = Set.unions . map simpleConstraintVars . unSimpleSystem

findHighestVar :: SimpleSystem -> Maybe Var
findHighestVar simpleSystem =
  let vars = simpleSystemVars simpleSystem
  in  if Set.null vars
        then Nothing
        else Just $ Set.findMax vars

nextAvailableVar :: SimpleSystem -> Var
nextAvailableVar simpleSystem =
  case findHighestVar simpleSystem of
    Just v -> v + 1
    Nothing -> 0

class CanBeSimpleSystem a where
  toSimpleSystem :: a -> SimpleSystem

instance CanBeSimpleSystem SimpleSystem where
  toSimpleSystem = id

instance CanBeSimpleSystem System where
  toSimpleSystem = undefined
