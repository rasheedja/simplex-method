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

import Control.Monad.Logger
  ( LogLevel (..)
  , LoggingT
  , MonadLogger
  , defaultOutput
  , logOtherN
  , runLoggingT
  , toLogStr
  )
import Data.Generics.Labels ()
import Data.List (nub, (\\))
import qualified Data.Map as Map
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
import System.IO (stdout)
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
combineVarLitMapSums = Map.unionWith (+)

-- | Sum coefficient maps, treating an empty list as the zero expression.
foldVarLitMap :: [VarLitMap] -> VarLitMap
foldVarLitMap = Map.unionsWith (+)

insertPivotObjectiveToDict :: PivotObjective -> Dict -> Dict
insertPivotObjectiveToDict objective = Map.insert objective.variable (DictValue {varMapSum = objective.function, constant = objective.constant})

showT :: (Show a) => a -> T.Text
showT = T.pack . show

-- | Log a message at the given level, exactly as given.
--
-- No timestamp is attached: reading a clock is the log sink's job, so the
-- solver stays free of @MonadIO@ and does no IO when logging is disabled.
-- For deterministic output the interesting axis is the pivot number, which the
-- solver puts in its own messages. Use 'runTimestampedStdoutLoggingT' to get
-- wall-clock timestamps back.
logMsg :: (MonadLogger m) => LogLevel -> T.Text -> m ()
logMsg = logOtherN

-- | Run a 'LoggingT' action, prefixing every emitted line with the current
-- ISO-8601 time.
--
-- This is where timestamps moved to when 'logMsg' stopped reading a clock. It
-- renders exactly like @runStdoutLoggingT@, only with the timestamp prepended,
-- so it is a drop-in replacement for callers that want the old output and do
-- not mind paying for the clock syscall on every message.
runTimestampedStdoutLoggingT :: LoggingT IO a -> IO a
runTimestampedStdoutLoggingT action =
  runLoggingT action $ \loc source level message -> do
    timestamp <- T.pack . iso8601Show <$> getCurrentTime
    defaultOutput stdout loc source level (toLogStr (timestamp <> ": ") <> message)
