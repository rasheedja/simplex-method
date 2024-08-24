-- |
-- Module: Linear.Expr.Types
-- Description: Types for linear expressions
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: jrasheed178@gmail.com
-- Stability: experimental
module Linear.Expr.Types where

import qualified Data.List.NonEmpty as NE
import GHC.Base (liftA2)
import GHC.Generics (Generic)
import Linear.Term.Types (Term, TermVarsOnly)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (suchThat)

newtype Expr = Expr {unExpr :: NE.NonEmpty Term}
  deriving
    ( Show
    , Read
    , Eq
    , Generic
    )

newtype ExprVarsOnly = ExprVarsOnly {unExprVarsOnly :: NE.NonEmpty TermVarsOnly}
  deriving
    ( Show
    , Read
    , Eq
    , Generic
    )

instance Arbitrary Expr where
  arbitrary = Expr . NE.fromList <$> arbitrary `suchThat` (not . null)
