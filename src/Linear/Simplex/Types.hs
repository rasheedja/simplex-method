-- |
-- Module      : Linear.Simplex.Types
-- Description : Custom types
-- Copyright   : (c) Junaid Rasheed, 2020-2023
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
module Linear.Simplex.Types where

import Control.Lens
import Data.Generics.Labels ()
import Data.List (sort)
import qualified Data.Map as M
import GHC.Generics (Generic)

type Var = Int

type SimplexNum = Rational

type SystemRow = PolyConstraint

type System = [SystemRow]

-- A 'Tableau' where the basic variable may be empty.
-- All non-empty basic vars are slack vars
data SystemWithSlackVarRow = SystemInStandardFormRow
  { mSlackVar :: Maybe Var
  -- ^ This is Nothing iff the row does not have a slack variable
  , row :: TableauRow
  }

type SystemWithSlackVars = [SystemWithSlackVarRow]

data FeasibleSystem = FeasibleSystem
  { dict :: Dict
  , slackVars :: [Var]
  , artificialVars :: [Var]
  , objectiveVar :: Var
  }
  deriving (Show, Read, Eq, Generic)

-- | The outcome of optimizing a single objective function.
data OptimisationOutcome
  = Optimal { varValMap :: VarLitMap }  -- ^ An optimal solution was found
  | Unbounded                            -- ^ The objective is unbounded
  deriving (Show, Read, Eq, Generic)

-- | Result for a single objective function optimization.
data ObjectiveResult = ObjectiveResult
  { objectiveFunction :: ObjectiveFunction  -- ^ The objective that was optimized
  , outcome :: OptimisationOutcome           -- ^ The optimization outcome
  }
  deriving (Show, Read, Eq, Generic)

-- | Complete result of the two-phase simplex method.
-- Contains feasibility information and results for all requested objectives.
data SimplexResult = SimplexResult
  { feasibleSystem :: Maybe FeasibleSystem  -- ^ The feasible system (Nothing if infeasible)
  , objectiveResults :: [ObjectiveResult]   -- ^ Results for each objective (empty if infeasible)
  }
  deriving (Show, Read, Eq, Generic)

type VarLitMap = M.Map Var SimplexNum

-- | List of variables with their 'SimplexNum' coefficients.
--   There is an implicit addition between elements in this list.
--
--   Example: [Var "x" 3, Var "y" -1, Var "z" 1] is equivalent to 3x + (-y) + z.
type VarLitMapSum = VarLitMap

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
data ObjectiveFunction = Max {objective :: VarLitMapSum} | Min {objective :: VarLitMapSum}
  deriving (Show, Read, Eq, Generic)

-- | TODO: Maybe we want this type
-- TODO: A better/alternative name
data Equation = Equation
  { lhs :: VarLitMapSum
  , rhs :: SimplexNum
  }

-- | Value for 'Tableau'. lhs = rhs.
data TableauRow = TableauRow
  { lhs :: VarLitMapSum
  , rhs :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

-- | A simplex 'Tableu' of equations.
--   Each entry in the map is a row.
type Tableau = M.Map Var TableauRow

-- | Values for a 'Dict'.
data DictValue = DictValue
  { varMapSum :: VarLitMapSum
  , constant :: SimplexNum
  }
  deriving (Show, Read, Eq, Generic)

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

-- | Domain specification for a variable's bounds.
-- Variables not in the VarDomainMap are assumed to be Unbounded (both bounds Nothing).
-- 
-- Bounds semantics:
--   * @lowerBound = Just L@ means var >= L
--   * @lowerBound = Nothing@ means no lower bound (var can be arbitrarily negative)
--   * @upperBound = Just U@ means var <= U  
--   * @upperBound = Nothing@ means no upper bound (var can be arbitrarily positive)
--
-- Note: @Bounded Nothing Nothing@ is equivalent to unbounded. Use the smart constructors
-- ('unbounded', 'nonNegative', etc.) for clarity.
data VarDomain = Bounded 
  { lowerBound :: Maybe SimplexNum  -- ^ Lower bound (Nothing = -∞)
  , upperBound :: Maybe SimplexNum  -- ^ Upper bound (Nothing = +∞)
  }
  deriving stock (Show, Read, Eq, Generic)

-- | Smart constructor for an unbounded variable (no lower or upper bound).
-- The variable can take any real value.
unbounded :: VarDomain
unbounded = Bounded Nothing Nothing

-- | Smart constructor for a non-negative variable (var >= 0).
-- This is the standard simplex assumption.
nonNegative :: VarDomain
nonNegative = Bounded (Just 0) Nothing

-- | Smart constructor for a variable with only a lower bound (var >= L).
lowerBoundOnly :: SimplexNum -> VarDomain
lowerBoundOnly l = Bounded (Just l) Nothing

-- | Smart constructor for a variable with only an upper bound (var <= U).
upperBoundOnly :: SimplexNum -> VarDomain
upperBoundOnly u = Bounded Nothing (Just u)

-- | Smart constructor for a variable with both lower and upper bounds (L <= var <= U).
boundedRange :: SimplexNum -> SimplexNum -> VarDomain
boundedRange l u = Bounded (Just l) (Just u)

-- | Map from variables to their domain specifications.
-- Variables not in this map are assumed to be Unbounded.
newtype VarDomainMap = VarDomainMap { unVarDomainMap :: M.Map Var VarDomain }
  deriving stock (Show, Read, Eq, Generic)

-- | Transformations applied to variables to ensure they satisfy the non-negativity requirement.
data VarTransform 
  = AddLowerBound 
      { var :: !Var
      , bound :: !SimplexNum 
      } -- ^ var >= bound where bound > 0. Adds GEQ constraint to system.
  | AddUpperBound
      { var :: !Var
      , bound :: !SimplexNum
      } -- ^ var <= bound. Adds LEQ constraint to system.
  | Shift 
      { originalVar :: !Var
      , shiftedVar :: !Var  
      , shiftBy :: !SimplexNum 
      } -- ^ originalVar = shiftedVar + shiftBy, where shiftBy < 0. After solving: originalVar = shiftedVar + shiftBy
  | Split 
      { originalVar :: !Var
      , posVar :: !Var
      , negVar :: !Var 
      } -- ^ originalVar = posVar - negVar, both posVar and negVar >= 0
  deriving stock (Show, Read, Eq, Generic)
                  


