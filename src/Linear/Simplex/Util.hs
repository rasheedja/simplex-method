-- |
-- Module      : Linear.Simplex.Util
-- Description : Helper functions
-- Copyright   : (c) Junaid Rasheed, 2020-2023
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
--
-- Helper functions for performing the two-phase simplex method.
module Linear.Simplex.Util where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (LogLevel (..), MonadLogger, logDebug, logError, logInfo, logWarn)
import Data.Generics.Labels ()
import Data.List (nub, (\\))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as MapMerge
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Linear.Simplex.Types
  ( Dict
  , DictValue (..)
  , ObjectiveFunction (..)
  , PivotObjective (..)
  , PolyConstraint (..)
  , SimplexNum
  , Tableau
  , TableauRow (..)
  , Var
  , VarLitMap
  , VarLitMapSum
  )
import Prelude hiding (EQ)

-- | Is the given 'ObjectiveFunction' to be 'Max'imized?
isMax :: ObjectiveFunction -> Bool
isMax (Max _) = True
isMax (Min _) = False

-- | Simplifies a system of 'PolyConstraint's,
--  then reducing 'LEQ' and 'GEQ' with same LHS and RHS (and other similar situations) into 'EQ',
--  and finally removing duplicate elements using 'nub'.
simplifySystem :: [PolyConstraint] -> [PolyConstraint]
simplifySystem = nub . reduceSystem
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

-- | Converts a 'Dict' to a 'Tableau'.
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

-- | Converts a 'Tableau' to a 'Dict'.
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
                        (Nothing, Nothing) -> error "Reached unreachable branch in foldVarLitMap"
                    )
            )
            combinedVars
  in  foldVarLitMap $ combinedVarMap : vms

insertPivotObjectiveToDict :: PivotObjective -> Dict -> Dict
insertPivotObjectiveToDict objective = Map.insert objective.variable (DictValue {varMapSum = objective.function, constant = objective.constant})

showT :: (Show a) => a -> T.Text
showT = T.pack . show

logMsg :: (MonadIO m, MonadLogger m) => LogLevel -> T.Text -> m ()
logMsg lvl msg = do
  currTime <- T.pack . iso8601Show <$> liftIO getCurrentTime
  let msgToLog = currTime <> ": " <> msg
  case lvl of
    LevelDebug -> $logDebug msgToLog
    LevelInfo -> $logInfo msgToLog
    LevelWarn -> $logWarn msgToLog
    LevelError -> $logError msgToLog
    LevelOther otherLvl -> error "logMsg: LevelOther is not implemented"
