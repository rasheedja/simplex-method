-- |
-- Module      : Linear.Constraint.Generic.Types
-- Description : Types for constraints in linear programming problems
-- Copyright   : (c) Junaid Rasheed, 2020-2024
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
module Linear.Constraint.Generic.Types where

import Control.Applicative (liftA2)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary, arbitrary, genericShrink, oneof)

data GenericConstraint a b = a :<= b | a :>= b | a :== b
  deriving (Show, Read, Eq, Generic)

instance (Arbitrary a, Arbitrary b) => Arbitrary (GenericConstraint a b) where
  arbitrary =
    oneof
      [ liftA2 (:<=) arbitrary arbitrary
      , liftA2 (:>=) arbitrary arbitrary
      , liftA2 (:==) arbitrary arbitrary
      ]

getGenericConstraintLHS :: GenericConstraint a b -> a
getGenericConstraintLHS (a :<= _) = a
getGenericConstraintLHS (a :>= _) = a
getGenericConstraintLHS (a :== _) = a

getGenericConstraintRHS :: GenericConstraint a b -> b
getGenericConstraintRHS (_ :<= b) = b
getGenericConstraintRHS (_ :>= b) = b
getGenericConstraintRHS (_ :== b) = b
