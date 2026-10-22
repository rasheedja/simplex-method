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
import Data.List (mapAccumL)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Linear.Simplex.Types
  ( Dict
  , DictValue (..)
  , ObjectiveFunction (..)
  , PolyConstraint (..)
  , SimplexNum
  , Tableau
  , TableauRow (..)
  , VarLitMap
  , VarLitMapSum
  )
import System.IO (stdout)
import Prelude hiding (EQ)

-- | Is the given 'ObjectiveFunction' to be 'Max'imized?
isMax :: ObjectiveFunction -> Bool
isMax (Max _) = True
isMax (Min _) = False

-- | The relation of a 'PolyConstraint'.
--  'simplifySystem' groups constraints by relation, so it needs to talk about
--  the relation on its own, separately from the 'PolyConstraint' that carries it.
data RelationKind = RelationLEQ | RelationGEQ | RelationEQ
  deriving (Eq)

-- | Everything 'simplifySystem' needs to know about all the constraints that share
--  one @(lhs, rhs)@ key before it can decide what that key reduces to.
data RelationSummary = RelationSummary
  { firstKind :: RelationKind
  -- ^ The relation of the first constraint carrying the key
  , otherKindPresent :: Bool
  -- ^ Does a constraint with a different relation carry the key too?
  , firstKindCount :: Int
  -- ^ How many constraints with 'firstKind' carry the key, counted up to 2
  }

-- | The @(lhs, rhs)@ pair that 'simplifySystem' groups 'PolyConstraint's by.
type ConstraintKey = (VarLitMapSum, SimplexNum)

constraintKey :: PolyConstraint -> ConstraintKey
constraintKey (LEQ lhs rhs) = (lhs, rhs)
constraintKey (GEQ lhs rhs) = (lhs, rhs)
constraintKey (EQ lhs rhs) = (lhs, rhs)

constraintKind :: PolyConstraint -> RelationKind
constraintKind (LEQ _ _) = RelationLEQ
constraintKind (GEQ _ _) = RelationGEQ
constraintKind (EQ _ _) = RelationEQ

-- | The summary of a key carried by a single constraint with the given relation.
singleRelationSummary :: RelationKind -> RelationSummary
singleRelationSummary kind = RelationSummary {firstKind = kind, otherKindPresent = False, firstKindCount = 1}

-- | Add another constraint carrying the same key to the key's summary,
--  keeping the relation of the constraint that was seen first.
mergeRelationSummary :: RelationSummary -> RelationSummary -> RelationSummary
mergeRelationSummary new older =
  RelationSummary
    { firstKind = older.firstKind
    , otherKindPresent = older.otherKindPresent || new.firstKind /= older.firstKind
    , firstKindCount =
        if new.firstKind == older.firstKind
          then min 2 (older.firstKindCount + 1)
          else older.firstKindCount
    }

-- | Simplifies a system of 'PolyConstraint's by grouping the constraints that share
--  a @(lhs, rhs)@ key and reducing each group, in @O(n log n)@ rather than by
--  rescanning the remaining constraints for every constraint in the system.
--
--  For each @(lhs, rhs)@ key, let @k@ be the relation of the first constraint that
--  carries the key. The group reduces to:
--
--  * one @EQ lhs rhs@, when the group holds a constraint of another relation as well,
--    i.e. a 'LEQ' next to a 'GEQ' (in either order) or an 'EQ' next to a 'LEQ' or a
--    'GEQ', or when @k@ is 'EQ' already;
--  * the single relation @k@ otherwise, which also collapses repetitions of @k@.
--
--  One corner of the grouping is inherited from the reference implementation
--  (@nub . reduceSystem@) this replaces: when @k@ is 'LEQ' (or 'GEQ') and the group
--  holds another relation as well as a second 'LEQ' (respectively 'GEQ'), that second
--  constraint is not absorbed by the reduction and survives untouched, because the
--  duplicates collapse only when they are equal as a whole. So
--  @[LEQ l r, GEQ l r, LEQ l r]@ is @[EQ l r, LEQ l r]@ and not @[EQ l r]@.
--
--  Every surviving constraint is emitted at the position of the constraint it is
--  derived from, so the first-occurrence order of the input is kept. Slack and
--  artificial variables are allocated in constraint order, so that order decides
--  the Bland tie-breaks.
simplifySystem :: [PolyConstraint] -> [PolyConstraint]
simplifySystem constraints = concat (snd (mapAccumL emitAtKey Map.empty constraints))
  where
    summaries :: Map.Map ConstraintKey RelationSummary
    summaries =
      Map.fromListWith
        mergeRelationSummary
        [(constraintKey constraint, singleRelationSummary (constraintKind constraint)) | constraint <- constraints]

    -- Emit a constraint only where the reduction keeps one. The accumulator counts
    -- how many constraints of each key's first relation have been emitted so far,
    -- which says whether this constraint is that relation's first or second one.
    emitAtKey :: Map.Map ConstraintKey Int -> PolyConstraint -> (Map.Map ConstraintKey Int, [PolyConstraint])
    emitAtKey seen constraint =
      let key = constraintKey constraint
          summary = Map.findWithDefault (singleRelationSummary (constraintKind constraint)) key summaries
          seenFirstKind = Map.findWithDefault 0 key seen
          seen' =
            if constraintKind constraint == summary.firstKind
              then Map.insert key (seenFirstKind + 1) seen
              else seen
      in  (seen', emitted summary seenFirstKind constraint)

    emitted :: RelationSummary -> Int -> PolyConstraint -> [PolyConstraint]
    emitted summary seenFirstKind constraint
      | constraintKind constraint /= summary.firstKind = []
      | seenFirstKind == 0 = [firstOccurrence summary constraint]
      | seenFirstKind == 1 && repeatsFirstKind summary = [constraint]
      | otherwise = []

    -- The first constraint of a key becomes an 'EQ' when the group holds another
    -- relation as well, and is otherwise kept as it is.
    firstOccurrence :: RelationSummary -> PolyConstraint -> PolyConstraint
    firstOccurrence summary constraint
      | summary.otherKindPresent || summary.firstKind == RelationEQ =
          let (lhs, rhs) = constraintKey constraint in EQ lhs rhs
      | otherwise = constraint

    -- A second constraint of the key's first relation survives when the reduction
    -- absorbed a constraint of another relation. An 'EQ' first relation never gets
    -- there: its later duplicates are equal to it and collapse into it.
    repeatsFirstKind :: RelationSummary -> Bool
    repeatsFirstKind summary =
      summary.otherKindPresent && summary.firstKind /= RelationEQ && summary.firstKindCount > 1

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
