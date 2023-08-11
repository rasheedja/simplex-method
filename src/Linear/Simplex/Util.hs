-- |
-- Module      : Linear.Simplex.Util
-- Description : Helper functions
-- Copyright   : (c) Junaid Rasheed, 2020-2022
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
--
-- Helper functions for performing the two-phase simplex method.
module Linear.Simplex.Util where

import Control.Lens
import Data.Bifunctor
import Data.Generics.Labels ()
import Data.Generics.Product (field)
import Data.List
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as MapMerge
import Data.Maybe (fromMaybe)
import Linear.Simplex.Types
import Prelude hiding (EQ)

-- | Is the given 'ObjectiveFunction' to be 'Max'imized?
isMax :: ObjectiveFunction -> Bool
isMax (Max _) = True
isMax (Min _) = False

-- | Simplifies a system of 'PolyConstraint's by first calling 'simplifyPolyConstraint',
--  then reducing 'LEQ' and 'GEQ' with same LHS and RHS (and other similar situations) into 'EQ',
--  and finally removing duplicate elements using 'nub'.
simplifySystem :: [PolyConstraint] -> [PolyConstraint]
simplifySystem = nub . reduceSystem . map simplifyPolyConstraint
  where
    reduceSystem :: [PolyConstraint] -> [PolyConstraint]
    reduceSystem [] = []
    -- Reduce LEQ with matching GEQ and EQ into EQ
    reduceSystem ((LEQ lhs rhs) : pcs) =
      let matchingConstraints =
            filter
              ( \case
                  GEQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  EQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  _ -> False
              )
              pcs
      in  if null matchingConstraints
            then LEQ lhs rhs : reduceSystem pcs
            else EQ lhs rhs : reduceSystem (pcs \\ matchingConstraints)
    -- Reduce GEQ with matching LEQ and EQ into EQ
    reduceSystem ((GEQ lhs rhs) : pcs) =
      let matchingConstraints =
            filter
              ( \case
                  LEQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  EQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  _ -> False
              )
              pcs
      in  if null matchingConstraints
            then GEQ lhs rhs : reduceSystem pcs
            else EQ lhs rhs : reduceSystem (pcs \\ matchingConstraints)
    -- Reduce EQ with matching LEQ and GEQ into EQ
    reduceSystem ((EQ lhs rhs) : pcs) =
      let matchingConstraints =
            filter
              ( \case
                  LEQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  GEQ lhs' rhs' -> lhs == lhs' && rhs == rhs'
                  _ -> False
              )
              pcs
      in  if null matchingConstraints
            then EQ lhs rhs : reduceSystem pcs
            else EQ lhs rhs : reduceSystem (pcs \\ matchingConstraints)

-- | Simplify an 'ObjectiveFunction' by first 'sort'ing and then calling 'foldSumVarConstMap' on the 'Vars'.
simplifyObjectiveFunction :: ObjectiveFunction -> ObjectiveFunction
-- simplifyObjectiveFunction (Max vars) = Max (foldSumVarConstMap (sort vars))
-- simplifyObjectiveFunction (Min vars) = Min (foldSumVarConstMap (sort vars))
simplifyObjectiveFunction (Max vars) = Max vars
simplifyObjectiveFunction (Min vars) = Min vars

-- | Simplify a 'PolyConstraint' by first 'sort'ing and then calling 'foldSumVarConstMap' on the 'Vars'.
simplifyPolyConstraint :: PolyConstraint -> PolyConstraint
-- simplifyPolyConstraint (LEQ vars rhs) = LEQ (foldSumVarConstMap (sort vars)) rhs
-- simplifyPolyConstraint (GEQ vars rhs) = GEQ (foldSumVarConstMap (sort vars)) rhs
-- simplifyPolyConstraint (EQ vars rhs) = EQ (foldSumVarConstMap (sort vars)) rhs
simplifyPolyConstraint (LEQ vars rhs) = LEQ vars rhs
simplifyPolyConstraint (GEQ vars rhs) = GEQ vars rhs
simplifyPolyConstraint (EQ vars rhs) = EQ vars rhs

-- | Add a sorted list of 'Vars's, folding where the variables are equal
-- foldSumVarConstMap :: VarLitMapSum -> VarLitMapSum
-- foldSumVarConstMap [] = []
-- foldSumVarConstMap v@[_] = v
-- foldSumVarConstMap (v1 : v2 : vcm) =
--   if v1 == v2
--     then
--       let c1 = v1 ^. #coeff
--           c2 = v2 ^. #coeff
--           newC = c1 + c2
--       in  if newC == 0
--             then foldSumVarConstMap vcm
--             else foldSumVarConstMap $ (v1 & #coeff .~ newC) : vcm
--     else v1 : foldSumVarConstMap (v2 : vcm)

-- | Get a map of the value of every variable in a 'Tableau'
-- displayTableauResults :: Tableau -> Map.Map Var SimplexNum
-- displayTableauResults = Map.fromList . map (\entry -> (entry ^. #basicVarName, entry ^. #rhs))

-- | Get a map of the value of every variable in a 'Dict'
-- displayDictionaryResults :: Dict -> Map.Map Var SimplexNum
-- displayDictionaryResults dict = displayTableauResults $ dictionaryFormToTableau dict

-- | Map the given 'Integer' variable to the given 'ObjectiveFunction', for entering into 'DictionaryForm'.
-- createObjectiveDict :: ObjectiveFunction -> Var -> DictObjective
-- createObjectiveDict (Max obj) objectiveVar =
--   DictObjective
--     { lhs = objectiveVar
--     , rhs =
--         DictEntryValue
--           { varMapSum = obj
--           , constant = 0 -- FIXME: was 1? prob should be 0
--           }
--     }
-- createObjectiveDict (Min obj) objectiveVar =
--   DictObjective
--     { lhs = objectiveVar
--     , rhs =
--         DictEntryValue
--           { varMapSum = Map.map negate obj
--           , constant = 0 -- FIXME: was 1?
--           }
--     }

-- data DictEntryValue = DictEntryValue
--   { varMapSum :: VarLitMapSum
--   , constant :: SimplexNum
--   }
--   deriving (Show, Read, Eq, Generic)

--   data TableauRowValue = TableauRowValue
--   { lhs :: VarLitMapSum
--   , rhs :: SimplexNum
--   }
--   deriving (Show, Read, Eq, Generic)

-- | Converts a 'Dict' to a 'Tableau' using 'dictEntryToTableauEntry'.
--  FIXME: maybe remove this line. The basic variables will have a coefficient of 1 in the 'Tableau'.
dictionaryFormToTableau :: Dict -> Tableau
dictionaryFormToTableau =
  Map.mapWithKey
    ( \basicVar (DictValue {..}) ->
        TableauRow
          { lhs = Map.insert basicVar 1 $ negate <$> varMapSum
          , rhs = constant
          }
    )

-- { objective =
--     let objecitveBasicVar = objective ^. #lhs
--         objectiveRow = objective ^. #rhs
--     in  TableauObjective
--           { basicVar = objecitveBasicVar
--           , row = dictEntryValueToTableauRowValue objecitveBasicVar objectiveRow
--           }
-- , rows = Map.mapWithKey dictEntryValueToTableauRowValue entries
-- }
-- where
-- \| Converts a 'DictEntry' to a 'TableauEntry'.
--  This is done by moving all non-basic variables from the right to the left.
--  The rational constant stays on the right.
--  FIXME: check The basic variables will have a coefficient of 1 in the 'TableauEntry'.

-- | Converts a 'Tableau' to a 'Dict' using 'tableauEntryToDictEntry'.
--  We do this by isolating the basic variable on the LHS, ending up with all non basic variables and a 'SimplexNum' constant on the RHS.
tableauInDictionaryForm :: Tableau -> Dict
tableauInDictionaryForm =
  Map.mapWithKey
    ( \basicVar (TableauRow {..}) ->
        let basicVarCoeff = fromMaybe 1 $ Map.lookup basicVar lhs
        in  DictValue
              { varMapSum =
                  Map.map
                    (\c -> negate c / basicVarCoeff)
                    $ Map.delete basicVar lhs
              , constant = rhs / basicVarCoeff
              }
    )

-- Dict
--   { objective =
--       let objecitveBasicVar = objective ^. #basicVar
--           objectiveRow = objective ^. #row
--       in  DictObjective
--             { lhs = objecitveBasicVar
--             , rhs = tableauRowValueToDictEntryValue objecitveBasicVar objectiveRow
--             }
--   , entries = Map.mapWithKey tableauRowValueToDictEntryValue rows
--   }
-- where
-- -- \| Converts a 'Tableau' to a 'Dict'.
-- --  We do this by isolating the basic variable on the LHS, ending up with all non basic variables and a 'SimplexNum' constant on the RHS.
-- --  FIXME: check The basic variables will have a coefficient of 1 in the 'DictEntry'.
-- tableauRowValueToDictEntryValue :: Var -> TableauRow -> DictEntryValue
-- tableauRowValueToDictEntryValue basicVarName (TableauRow {..}) =
--   DictEntryValue
--     { varMapSum =
--         Map.map
--           (\c -> negate c / basicVarCoeff)
--           $ Map.delete basicVarName lhs
--     , constant = rhs / basicVarCoeff
--     }
--   where
-- mBasicVarCoeff = Map.lookup basicVarName lhs
-- basicVarCoeff = fromMaybe 1 mBasicVarCoeff

-- | If this function is given 'Nothing', return 'Nothing'.
--  Otherwise, we 'lookup' the 'Integer' given in the first item of the pair in the map given in the second item of the pair.
--  This is typically used to extract the value of the 'ObjectiveFunction' after calling 'Linear.Simplex.Simplex.twoPhaseSimplex'.
extractObjectiveValue :: Maybe Result -> Maybe SimplexNum
extractObjectiveValue = fmap $ \result ->
  case Map.lookup (result ^. #objectiveVar) (result ^. #varValMap) of
    Nothing -> error "Objective not found in results when extracting objective value"
    Just r -> r

-- | Combines two 'VarLitMapSums together by summing values with matching keys
combineVarLitMapSums :: VarLitMapSum -> VarLitMapSum -> VarLitMapSum
combineVarLitMapSums =
  MapMerge.merge
    (MapMerge.mapMaybeMissing keepVal)
    (MapMerge.mapMaybeMissing keepVal)
    (MapMerge.zipWithMaybeMatched sumVals)
  where
    keepVal = const pure
    sumVals k v1 v2 = Just $ v1 + v2

-- -- | Apply a function to the objective function and another to each entry in a
-- --  'Dict'
-- applyDict :: (DictEntries -> DictEntries) -> (DictEntries -> DictEntries) -> Dict -> Dict
-- applyDict fObj fDict (Dict {..}) =
--   Dict
--     { objective = fObj objectiveFunction
--     , entries = fmap fDict entries
--     }

-- -- Apply a single function to all entries in a 'Dict', including the objective function
-- applyDictSimple :: (DictEntries -> DictEntries) -> Dict -> Dict
-- applyDictSimple f = applyDict f f

foldDictValue :: [DictValue] -> DictValue
foldDictValue [] = error "Empty list of DictValues given to foldDictValue"
foldDictValue [x] = x
foldDictValue (DictValue {varMapSum = vm1, constant = c1} : DictValue {varMapSum = vm2, constant = c2} : dvs) =
  let combinedDictValue =
        DictValue
          { varMapSum = foldVarLitMap [vm1, vm2]
          , constant = c1 + c2
          }
  in  foldDictValue $ combinedDictValue : dvs

-- type VarLitMap = M.Map Var SimplexNum

foldVarLitMap :: [VarLitMap] -> VarLitMap
foldVarLitMap [] = error "Empty list of VarLitMaps given to foldVarLitMap"
foldVarLitMap [x] = x
foldVarLitMap (vm1 : vm2 : vms) =
  let combinedVars = nub $ Map.keys vm1 <> Map.keys vm2

      combinedVarMap =
        Map.fromList $
          map
            ( \var ->
                let mVm1VarVal = Map.lookup var vm1
                    mVm2VarVal = Map.lookup var vm2
                in  ( var
                    , case (mVm1VarVal, mVm2VarVal) of
                        (Just vm1VarVal, Just vm2VarVal) -> vm1VarVal + vm2VarVal
                        (Just vm1VarVal, Nothing) -> vm1VarVal
                        (Nothing, Just vm2VarVal) -> vm2VarVal
                        (Nothing, Nothing) -> error "Reached unreachable branch in foldDictValue"
                    )
            )
            combinedVars
  in  foldVarLitMap $ combinedVarMap : vms

insertPivotObjectiveToDict :: PivotObjective -> Dict -> Dict
insertPivotObjectiveToDict objective dict = Map.insert (objective.variable) (DictValue {varMapSum = objective.function, constant = objective.constant}) dict
