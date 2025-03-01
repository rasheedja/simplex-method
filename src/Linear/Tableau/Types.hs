module Linear.Tableau.Types where

import qualified Data.Set as Set
import Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck as QC

import Linear.Var.Types (SimplexNum)
import Linear.Simplex.Solver.Types (Objective)
import Linear.CanonicalForm.Types (CanonicalForm)

-- TODO: Consider a type where the size of the list is specified
-- | A row for a @Tableau@
data TableauRow = TableauRow { coeffs :: ![SimplexNum]
                              -- ^ Variable coefficients
                             , rhs :: !SimplexNum
                              -- ^ Right-hand side constants
                             }
  deriving stock (Show, Read, Eq)

instance Arbitrary TableauRow where
  arbitrary =
    TableauRow <$> QC.listOf1 arbitrary <*> arbitrary

-- | Type representing a Simplex Tableau
data Tableau = Tableau { rows :: ![TableauRow]
                       , basicVars :: !(Set.Set Int) -- TODO: document the sizes are the same
                       -- ^ Column index of the basic vars for each row
                       }
  deriving stock (Show, Read, Eq)

instance Arbitrary Tableau where
  arbitrary = do
    n <- QC.choose (1, 5)
    rows <- QC.vectorOf n arbitrary
    basicVars <- Set.fromList <$> QC.vectorOf n (QC.choose (0, 10))
    pure $ Tableau rows basicVars

rowCoeffsLength :: TableauRow -> Int
rowCoeffsLength row = length row.coeffs

rowLength :: TableauRow -> Int
rowLength row = 1 + rowCoeffsLength row

-- | Perform a pivot operation on a @Tableau@
pivot :: Int       -- ^ Entering column index (0-based)
      -> Int       -- ^ Leaving row index (0-based)
      -> Tableau
      -> Tableau
pivot enteringCol leavingRowIdx tableau =
  let
    -- Split tableau into leaving row and others
    (beforeRows, pivotRow:afterRows) = splitAt leavingRowIdx tableau.rows
    pivotElemCoeff = getCoeff enteringCol pivotRow

    -- Normalize pivot row
    normalizedRow = TableauRow
      { coeffs = map (/ pivotElemCoeff) pivotRow.coeffs
      , rhs = pivotRow.rhs / pivotElemCoeff
      }

    -- Update all other rows
    adjustedRows = map (adjustRow normalizedRow enteringCol) (beforeRows ++ afterRows)
    (adjustedBefore, adjustedAfter) = splitAt leavingRowIdx adjustedRows
    newRows = adjustedBefore ++ [normalizedRow] ++ adjustedAfter

    -- Update basic variables
    newBasicVars = updateBasicVar leavingRowIdx enteringCol tableau.basicVars
  in
    Tableau
      { rows = newRows
      , basicVars = newBasicVars
      }
  where
    getCoeff col (TableauRow cs _) = cs !! col

    -- Eliminate entering col using gaussian elimination
    adjustRow normalizedRow enterCol' row =
      let multiplier = getCoeff enterCol' row
          adjustedCoeffs = zipWith (-) (coeffs row) (map (* multiplier) (coeffs normalizedRow))
          adjustedRHS = rhs row - multiplier * rhs normalizedRow
      in TableauRow adjustedCoeffs adjustedRHS

    updateBasicVar idx newVar vars =
      let (before, after) = (Set.take idx vars, Set.drop (idx + 1) vars)
      in before <> Set.fromList [newVar] <> after

mkTableau :: Objective -> CanonicalForm -> Tableau
mkTableau = undefined

