{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Util where

import qualified Simplex as S;
import Prelude hiding (EQ);
import Data.List
import Data.Bifunctor
import Data.Maybe (fromMaybe)

import Debug.Trace (trace)

-- data Opt = Max | Min

type VarConstMap = [(Integer, Rational)]

data PolyConstraint =
  LEQ VarConstMap Rational      | 
  GEQ VarConstMap Rational      | 
  EQ VarConstMap Rational       deriving (Show, Eq);
  
data ObjectiveFunction = Max VarConstMap | Min VarConstMap deriving Show

-- |Type representing a tableau of equations.
-- Each item in the list is a row. The first item
-- in the pair specifies which variable is basic
-- in the equation. The VarConstMap in the second
-- pair is a list of variables with their coefficients.
-- These variables are on the left side of the equation.
-- The right hand side of the equation is a Rational
-- constant.
type Tableau = [(Integer, (VarConstMap, Rational))]

-- |Type representing equations. Each item in the list
-- is one equation. The first item of the pair is the
-- basic variable, and is on the left hand side of the
-- equation with a coefficient of one. The right hand
-- side is represented using a list of variables. To
-- represent a rational coefficient on the right hand
-- side, we can use the variable -1.
type DictionaryForm = [(Integer, VarConstMap)]

isMax :: ObjectiveFunction -> Bool
isMax (Max _) = True
isMax (Min _) = False

getObjective :: ObjectiveFunction -> VarConstMap
getObjective (Max o) = o
getObjective (Min o) = o

-- |Add a sorted list of VarConstMaps, folding where the variables are equal
foldSumVarConstMap :: [(Integer, Rational)] -> [(Integer, Rational)]
foldSumVarConstMap []                          = []
foldSumVarConstMap [(v, c)]                    = [(v, c)]
foldSumVarConstMap ((v1, c1) : (v2, c2) : vcm) =
  if v1 == v2
    then foldSumVarConstMap $ (v1, c1 + c2) : vcm
    else (v1, c1) : foldSumVarConstMap ((v2, c2) : vcm)


varConstMapToLinearPoly :: VarConstMap -> S.Linear_poly
varConstMapToLinearPoly vcm = S.LinearPoly (S.Fmap_of_list (map (first S.nat_of_integer) vcm))

polyConstraintToConstraint :: PolyConstraint -> S.Constraint
polyConstraintToConstraint pc =
  case pc of
    LEQ vcm r -> S.LEQ (varConstMapToLinearPoly vcm) r
    GEQ vcm r -> S.GEQ (varConstMapToLinearPoly vcm) r
    EQ vcm r -> S.EQ (varConstMapToLinearPoly vcm) r

-- |Perform the two phase simplex method with a given objective function to maximize and a system of constraints
-- assumes objFunction and system is not empty. Returns the a pair with the first item being the variable representing
-- the objective function and the second item being the values of all variables appearing in the system (including the
-- objective function).
twoPhaseSimplex :: ObjectiveFunction -> [PolyConstraint] -> Maybe (Integer, [(Integer, Rational)])
twoPhaseSimplex objFunction system = 
  if null artificialVars
    then displayResults . dictionaryFormToTableau <$> simplexPivot (createObjectiveDict objFunction objectiveVar : systemWithBasicVarsAsDictionary)
    else 
      case simplexPivot (createObjectiveDict artificialObjective objectiveVar : systemWithBasicVarsAsDictionary) of
        Just phase1Dict ->
          let 
                eliminateArtificialVarsFromPhase1Tableau = map (second (filter (\(v, _) -> v `notElem` artificialVars))) phase1Dict

                phase2Objective = 
                  (foldSumVarConstMap . sort) $
                    concatMap
                    (\(var, coeff) ->
                      case lookup var phase1Dict of
                        Nothing -> [(var, coeff)]
                        Just row -> map (second (*coeff)) $ filter (\(var, _) -> var `notElem` artificialVars) row
                    )  
                    (getObjective objFunction)

                phase2ObjFunction = if isMax objFunction then Max phase2Objective else Min phase2Objective
          in
            case lookup objectiveVar eliminateArtificialVarsFromPhase1Tableau of
              Nothing -> trace "objective row not found in phase 1 tableau" Nothing
              Just row ->
                if fromMaybe 0 (lookup (-1) row) == 0 
                  then trace "starting phase 2" displayResults . dictionaryFormToTableau <$> simplexPivot (createObjectiveDict phase2ObjFunction objectiveVar : tail eliminateArtificialVarsFromPhase1Tableau)
                  else trace "rhs not zero after phase 1, thus original tableau is infeasible" Nothing
        Nothing ->
          trace "Phase 1 tableau was infeasible (?)" Nothing

  where
    createObjectiveDict :: ObjectiveFunction -> Integer -> (Integer, VarConstMap)
    createObjectiveDict (Max obj) objectiveVar = (objectiveVar, obj)
    createObjectiveDict (Min obj) objectiveVar = (objectiveVar, map (second negate) obj)

    displayResults :: Tableau -> (Integer, [(Integer, Rational)])
    displayResults tableau =
      (
        objectiveVar,
        case objFunction of
          Max _ -> 
            map 
            (second snd) 
            $ filter (\(basicVar,_) -> basicVar `notElem` slackVars ++ artificialVars) tableau
          Min _ -> 
            map -- We maximized -objVar, so we negate the objVar to get the final value
            (\(basicVar, row) -> if basicVar == objectiveVar then (basicVar, negate (snd row)) else (basicVar, snd row))
            $ filter (\(basicVar,_) -> basicVar `notElem` slackVars ++ artificialVars) tableau
      )

    maxVar =
      max
      (maximum $ map 
      (\case
          LEQ vcm _ -> maximum (map fst vcm)
          GEQ vcm _ -> maximum (map fst vcm)
          EQ vcm _  -> maximum (map fst vcm)
      ) 
      system)
      (maximum objFunctionVars) -- This is not logically needed since if a variable does not appear to the system, 
                                      -- we can set this variable to infinity (since we assume all variables are >=0).
                                      -- But, this is safer.  

    (systemWithSlackVars, slackVars)      = systemInStandardForm system maxVar []

    maxVarWithSlackVars = if null slackVars then maxVar else maximum slackVars

    (systemWithBasicVars, artificialVars) = systemWithArtificialVars systemWithSlackVars maxVarWithSlackVars 

    finalMaxVar        = if null artificialVars then maxVarWithSlackVars else maximum artificialVars

    objectiveVar  = finalMaxVar + 1

    artificialObjective = createArtificialObjective systemWithBasicVarsAsDictionary artificialVars

    systemWithBasicVarsAsDictionary = tableauInDictionaryForm systemWithBasicVars

    objFunctionVars = map (fst) (getObjective objFunction)

    -- |System in standard form, a system of only equations. Add slack vars where necessary. This may give you
    -- an infeasible system if slack vars are negative when original variables are zero. If a constraint 
    -- is already EQ, set the basic var to Nothing. Final system is a list of equalities for the given system. 
    -- To be feasible, all vars must be >= 0.
    systemInStandardForm :: [PolyConstraint] -> Integer -> [Integer] -> ([(Maybe Integer, PolyConstraint)], [Integer])
    systemInStandardForm []  _       sVars = ([], sVars)
    systemInStandardForm (EQ v r : xs) maxVar sVars = ((Nothing, EQ v r) : newSystem, newSlackVars) 
      where
        (newSystem, newSlackVars) = systemInStandardForm xs maxVar sVars
    systemInStandardForm (LEQ v r : xs) maxVar  sVars = ((Just newSlackVar, EQ (v ++ [(newSlackVar, 1)]) r) : newSystem, newSlackVars)
      where
        newSlackVar = maxVar + 1
        (newSystem, newSlackVars) = systemInStandardForm xs newSlackVar (newSlackVar : sVars)
    systemInStandardForm (GEQ v r : xs) maxVar  sVars = ((Just newSlackVar, EQ (v ++ [(newSlackVar, -1)]) r) : newSystem, newSlackVars)
      where
        newSlackVar = maxVar + 1
        (newSystem, newSlackVars) = systemInStandardForm xs newSlackVar (newSlackVar : sVars)

    -- |Add artificial vars to a system.
    -- Artificial vars are added when:
    --  Basic var is Nothing (When the original constraint was already an EQ)
    --  Slack var is equal to a negative value (this is infeasible, all vars need to be >= 0)
    --  Final system will be a feasible artificial system.
    -- We keep track of artificial vars so they can be eliminated once phase 1 is complete
    -- If an artificial var would normally be negative, we negate the row so we can keep artificial variables equal to 1
    systemWithArtificialVars :: [(Maybe Integer, PolyConstraint)] -> Integer -> (Tableau, [Integer])
    systemWithArtificialVars [] _                                = ([],[])
    systemWithArtificialVars ((mVar, EQ v r) : pcs) maxVar  =
      case mVar of
        Nothing ->
          if r >= 0 
            then 
              ((newArtificialVar, (v ++ [(newArtificialVar, 1)], r)) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
            else 
              ((newArtificialVar, (v ++ [(newArtificialVar, -1)], r)) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
        Just basicVar ->
          case lookup basicVar v of
            Just basicVarCoeff ->
              if r >= 0
                then 
                  if basicVarCoeff >= 0
                    then ((basicVar, (v, r)) : newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                    else ((newArtificialVar, (v ++ [(newArtificialVar, 1)], r)) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar) -- Slack var is negative, r is positive (when original constraint was GEQ)
                else 
                  if basicVarCoeff <= 0
                    then ((basicVar, (v, r)) : newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                    else 
                      trace (show v ++ "\n" ++ show r ++ "\n" ++ show basicVar ++ "\n\n\n\n")
                      ((newArtificialVar, (v ++ [(newArtificialVar, -1)], r)) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar) -- Slack var is negative, r is negative (when original constraint was LEQ)
      where
        newArtificialVar = maxVar + 1
        negatedV = map (second negate) v

        (newSystemWithNewMaxVar, artificialVarsWithNewMaxVar) = systemWithArtificialVars pcs newArtificialVar

        (newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar) = systemWithArtificialVars pcs maxVar

    -- |Create an artificial objective using the given list of artificialVars and the dictionary.
    -- The artificial objective is the negated sum of all artificial vars.
    createArtificialObjective :: DictionaryForm -> [Integer] -> ObjectiveFunction
    createArtificialObjective rows artificialVars = Max negatedSumWithoutArtificialVars
      where
        rowsToAdd = filter (\(i, _) -> i `elem` artificialVars) rows
        negatedRows = map (\(_, vcm) -> map (second negate) vcm) rowsToAdd
        negatedSum = foldSumVarConstMap ((sort . concat) negatedRows) 
        negatedSumWithoutArtificialVars = filter (\(v, _) -> v `notElem` artificialVars) negatedSum

-- |Perform the simplex pivot algorithm on a system with basic vars and the first row being the objective function
simplexPivot :: DictionaryForm -> Maybe DictionaryForm
simplexPivot dictionary = 
  trace (show dictionary) $
  case mostPositive (head dictionary) of
    Nothing -> 
      trace "all neg \n"
      trace (show dictionary)
      Just dictionary
    Just pivotNonBasicVar -> 
      let
        mPivotBasicVar = ratioTest (tail dictionary) pivotNonBasicVar Nothing Nothing
      in
        case mPivotBasicVar of
          Nothing -> trace ("Ratio test failed on non-basic var: " ++ show pivotNonBasicVar ++ "\n" ++ show dictionary) Nothing
          Just pivotBasicVar -> 
            trace "one pos \n"
            trace (show dictionary)
            simplexPivot $ map (\(S.Nat v, S.LinearPoly (S.Fmap_of_list vcm)) -> (v, map (first S.integer_of_nat) vcm)) newDictionary 
            where
              newDictionary = 
                S.pivot_tableau_code 
                (S.Nat pivotBasicVar) 
                (S.Nat pivotNonBasicVar) 
                $ map (bimap S.Nat (S.LinearPoly . S.Fmap_of_list . map (first S.Nat))) dictionary
  where
    ratioTest :: DictionaryForm -> Integer -> Maybe Integer -> Maybe Rational -> Maybe Integer
    ratioTest []                    _               mCurrentMinBasicVar _           = mCurrentMinBasicVar
    ratioTest ((basicVar, lp) : xs) mostNegativeVar mCurrentMinBasicVar mCurrentMin =
      case lookup mostNegativeVar lp of
        Nothing                         -> ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin
        Just currentCoeff ->
          case lookup (-1) lp of
            Nothing  -> trace "RHS not found in row in dict form" Nothing
            Just rhs ->
              if currentCoeff >= 0 || rhs < 0
                then 
                  -- trace (show currentCoeff)
                  ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin -- rhs was already in right side in original tableau, so should be above zero
                                                                               -- Coeff needs to be negative since it has been moved to the RHS
                else
                  case mCurrentMin of
                    Nothing         -> ratioTest xs mostNegativeVar (Just basicVar) (Just (rhs / currentCoeff))
                    Just currentMin ->
                      if (rhs / currentCoeff) >= currentMin
                        then ratioTest xs mostNegativeVar (Just basicVar) (Just (rhs / currentCoeff))
                        else ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin

    mostPositive :: (Integer, VarConstMap) -> Maybe Integer
    mostPositive (_, lp) = 
      case findLargestCoeff lp Nothing of
        Just (largestVar, largestCoeff) ->
          if largestCoeff <= 0 
            then Nothing
            else Just largestVar
        Nothing -> trace "No variables in first row when looking for most positive" Nothing

      where
        findLargestCoeff :: VarConstMap -> Maybe (Integer, Rational) -> Maybe (Integer, Rational)
        findLargestCoeff [] mCurrentMax                  = mCurrentMax
        findLargestCoeff ((var, coeff) : xs) mCurrentMax = 
          if var == (-1) 
            then findLargestCoeff xs mCurrentMax
            else 
              case mCurrentMax of
                Nothing         -> findLargestCoeff xs (Just (var, coeff))
                Just currentMax ->
                  if snd currentMax >= coeff 
                    then findLargestCoeff xs mCurrentMax
                    else findLargestCoeff xs (Just (var, coeff))

-- |Converts a Tableau to dictionary form.
-- We do this by isolating the basic variable on the left,
-- ending up with all non basic variables and a rational constant
-- on the right. (-1) is used to represent the rational constant.
tableauInDictionaryForm :: Tableau -> DictionaryForm
tableauInDictionaryForm []                      = []
tableauInDictionaryForm ((basicVar, (vcm, r)) : rows)  =
  (basicVar, (-1, r / basicCoeff) : map (\(v, c) -> (v, negate c / basicCoeff)) nonBasicVars) : tableauInDictionaryForm rows
  where
    basicCoeff = if null basicVars then 1 else snd $ head basicVars
    (basicVars, nonBasicVars) = partition (\(v, _) -> v == basicVar) vcm

-- |Converts a list of rows in dictionary form to a tableau.
-- This is done by moving all non-basic variables from the right
-- to the left. The rational constant stays on the right.
-- The basic variable will have a coefficient of 1 in the tableau.
dictionaryFormToTableau :: DictionaryForm -> Tableau
dictionaryFormToTableau [] = []
dictionaryFormToTableau ((basicVar, row) : rows) = 
    (basicVar, ((basicVar, 1) : map (second negate) nonBasicVars, r)) : dictionaryFormToTableau rows
  where
    (rationalConstant, nonBasicVars) = partition (\(v,_) -> v == (-1)) row
    r = if null rationalConstant then 0 else (snd . head) rationalConstant -- If there is no rational constant found in the right side, the rational constant is 0.
