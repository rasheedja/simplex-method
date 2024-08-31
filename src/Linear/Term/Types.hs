-- |
-- Module      : Linear.Term.Types
-- Description : Types for linear terms
-- Copright    : (c) Junaid Rasheed, 2020-2024
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
module Linear.Term.Types where

import GHC.Generics (Generic)
import Linear.Var.Types (SimplexNum, Var)
import Test.QuickCheck (Arbitrary (..), genericShrink, oneof)

data Term
  = ConstTerm {constant :: SimplexNum}
  | CoeffTerm {coeff :: SimplexNum, var :: Var}
  | VarTerm {var :: Var}
  deriving (Show, Read, Eq, Ord, Generic)

data TermVarsOnly
  = VarTermVO {var :: Var}
  | CoeffTermVO {coeff :: SimplexNum, var :: Var}
  deriving (Show, Read, Eq, Generic)

instance Arbitrary Term where
  arbitrary =
    oneof
      [ ConstTerm <$> arbitrary
      , CoeffTerm <$> arbitrary <*> arbitrary
      , VarTerm <$> arbitrary
      ]

instance Arbitrary TermVarsOnly where
  arbitrary =
    oneof
      [ VarTermVO <$> arbitrary
      , CoeffTermVO <$> arbitrary <*> arbitrary
      ]
