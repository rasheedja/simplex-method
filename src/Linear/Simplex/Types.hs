-- |
-- Module      : Linear.Simplex.Types
-- Description : Custom types
-- Copyright   : (c) Junaid Rasheed, 2020-2022
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
module Linear.Simplex.Types where

import Control.Lens
import Data.Generics.Labels ()
import Data.List (sort)
import Data.Map qualified as M
import GHC.Generics (Generic)

type Var = Int

-- TODO: Experiment with speed vs string vars type Var = Int
-- TODO: Could also just use (Eq var => var) directly

type SimplexNum = Rational

type SystemRow = PolyConstraint

type System = [SystemRow]

-- Basically, a tableau where the basic variable may be empty.
-- All non-empty basic vars are slack vars
data SystemWithSlackVarRow = SystemInStandardFormRow
  { mSlackVar :: Maybe Var
  -- ^ This is Nothing iff the row does not have a slack variable
  , row :: TableauRow
  }

type SystemWithSlackVars = [SystemWithSlackVarRow]

-- data SystemInStandardForm = SystemInStandardForm
-- { mBasicVar :: Maybe Var}

-- type SystemInStandardForm

-- data VarTerm = VarTerm
--   { name :: Var
--   , coeff :: SimplexNum
--   }
--   deriving (Show, Read, Eq, Generic)

data FeasibleSystem = FeasibleSystem
  { dict :: Dict
  , slackVars :: [Var]
  , artificialVars :: [Var]
  , objectiveVar :: Var
  } deriving (Show, Read, Eq, Generic)

data Result = Result
  { objectiveVar :: Var
  , varValMap :: VarLitMap
  -- TODO:
  -- Maybe VarLitMap
  -- , feasible :: Bool
  -- , optimisable :: Bool
  }
  deriving (Show, Read, Eq, Generic)

data SimplexMeta = SimplexMeta
  { objective :: ObjectiveFunction
  , feasibleSystem :: Maybe FeasibleSystem
  , optimisedResult :: Maybe Result
  }

type VarLitMap = M.Map Var SimplexNum

-- TURN THIS INTO A FUNCTION
-- instance Ord VarTerm where
--   x <= y = (x ^. #name) <= (y ^. #name)

-- | List of variables with their 'SimplexNum' coefficients.
--   There is an implicit addition between elements in this list.
--
--   Example: [Var "x" 3, Var "y" -1, Var "z" 1] is equivalent to 3x + (-y) + z.
type VarLitMapSum = VarLitMap

-- type VarLitMapSum = [VarLitMapSumEntry]

-- data VarLitMapSumEntry =
--   VarLitMapSumEntry
--     { name :: Var
--     , coeff :: SimplexNum
--     }

-- TODO: newtype VarTermSum = VarTermSum [VarTerm]
-- TODO: similar for other aliases

-- | For specifying constraints in a system.
--   The LHS is a 'Vars', and the RHS, is a 'SimplexNum' number.
--   LEQ [(1, 2), (2, 1)] 3.5 is equivalent to 2x1 + x2 <= 3.5.
--   Users must only provide positive integer variables.
--
--   Example: LEQ [Var "x" 3, Var "y" -1, Var "x" 1] 12.3 is equivalent to 3x + (-y) + x <= 12.3.
data PolyConstraint
  = LEQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
  | GEQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
  | EQ {lhs :: VarLitMapSum, rhs :: SimplexNum}
  deriving (Show, Read, Eq, Generic)

-- | Create an objective function.
--   We can either 'Max'imize or 'Min'imize a 'VarTermSum'.
-- TODO: Can the objective function contain a constant?
-- It can, but it's not useful. We just care about minimising/maximising vars,
-- not the actual value of the objective function
data ObjectiveFunction = Max {objective :: VarLitMapSum} | Min {objective :: VarLitMapSum} deriving (Show, Read, Eq, Generic)

-- | TODO: Maybe we want this type
-- TODO: A better/alternative name
data Equation = Equation
  { lhs :: VarLitMapSum
  , rhs :: SimplexNum
  }

-- | value for entry. lhs = rhs. TODO: finish
data TableauRow = TableauRow
  { lhs :: VarLitMapSum
  , rhs :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

-- | Entry for a simplex 'Tableau' of equations.
--   The LHS' is a 'VarTermSum'.
--   The RHS of the equation is a 'SimplexNum' constant.
--   The LHS is equal to the RHS.
-- type TableauRows = M.Map Var TableauRow

-- data TableauObjective = TableauObjective { basicVar :: Var, row :: TableauRow } deriving (Show, Read, Eq, Generic)

-- data TableauEntry = TableauEntry
--   { basicVarName :: Var
--   , lhs :: VarLitMapSum
--   , rhs :: SimplexNum
--   }
--   deriving (Show, Read, Eq, Generic)

-- | A simplex 'Tableu' of equations.
--   Each element in the list is a row.
type Tableau = M.Map Var TableauRow

-- data Tableau = Tableau
--   { objective :: TableauObjective
--   , rows :: TableauRows
--   }
--   deriving (Show, Read, Eq, Generic)

-- | Values for a 'DictEntry'. TODO: varMapSum + constant
-- TODO: DictValue -> DictRow
data DictValue = DictValue
  { varMapSum :: VarLitMapSum
  , constant :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

-- | A single entry for a simplex `Dict`.
--   The LHS is a `Var` and specifies a basic variable.
--   The RHS is a 'DictEquation'.
--   The LHS is equal to the RHS.
-- type DictEntries = M.Map Var DictEntryValue

-- data DictObjective = DictObjective { lhs :: Var, rhs :: DictEntryValue } deriving (Show, Read, Eq, Generic)

-- data DictEntry = DictEntry
--   { lhs :: Var
--   , rhs :: DictEquation
--   }
--   deriving (Show, Read, Eq, Generic)

-- | A simplex 'Dict'
--   One quation represents the objective function.
--   Each pair in the list is one equation in the system we're working with.
-- data Dict = Dict
--   { objective :: DictObjective
--   , entries :: DictEntries
--   }
--   deriving (Show, Read, Eq, Generic)
type Dict = M.Map Var DictValue

data PivotObjective = PivotObjective
  { variable :: Var
  , function :: VarLitMapSum
  , constant :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)
