-- |
-- Module: Linear.Expr.Types
-- Description: Types for linear expressions
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: jrasheed178@gmail.com
-- Stability: experimental
module Linear.Expr.Types where

import GHC.Generics (Generic)
import Linear.Term.Types (Term, TermVarsOnly)
import Test.QuickCheck (Arbitrary (..))

-- treat empty expr as 0
-- Consider a version with a num instance, use + and * operators for the input
newtype Expr = Expr {unExpr :: [Term]}
  deriving
    ( Show
    , Read
    , Eq
    , Generic
    )

newtype ExprVarsOnly = ExprVarsOnly {unExprVarsOnly :: [TermVarsOnly]}
  deriving
    ( Show
    , Read
    , Eq
    , Generic
    )

instance Arbitrary Expr where
  arbitrary = Expr <$> arbitrary

instance Arbitrary ExprVarsOnly where
  arbitrary = ExprVarsOnly <$> arbitrary
