{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Util where

import Simplex;
import Prelude;
import Data.List
import Control.Arrow

import Debug.Trace (trace)

data Opt = Max | Min

type VarConstMap = [(Integer, Rational)]

data PolyConstraint =
  LT VarConstMap Rational       | 
  GT VarConstMap Rational       | 
  LEQ VarConstMap Rational      | 
  GEQ VarConstMap Rational      | 
  EQ VarConstMap Rational       deriving (Show, Eq);

getRhs :: PolyConstraint -> Rational
getRhs (Util.LT _ r) = r
getRhs (Util.GT _ r) = r
getRhs (Util.LEQ _ r) = r
getRhs (Util.GEQ _ r) = r
getRhs (Util.EQ _ r) = r

varConstMapToLinearPoly :: VarConstMap -> Linear_poly
varConstMapToLinearPoly vcm = LinearPoly (Fmap_of_list (map (\(v,r) -> (nat_of_integer v, r)) vcm))

polyConstraintToConstraint :: PolyConstraint -> Constraint
polyConstraintToConstraint pc =
  case pc of
    Util.LT vcm r -> Simplex.LT (varConstMapToLinearPoly vcm) r
    Util.GT vcm r -> Simplex.GT (varConstMapToLinearPoly vcm) r
    Util.LEQ vcm r -> Simplex.LEQ (varConstMapToLinearPoly vcm) r
    Util.GEQ vcm r -> Simplex.GEQ (varConstMapToLinearPoly vcm) r
    Util.EQ vcm r -> Simplex.EQ (varConstMapToLinearPoly vcm) r

showSimplexResult :: Sum [Nat] (Mapping Nat Rational) -> (Bool, [Maybe (Integer, Integer)])
showSimplexResult result =
  case result of
    Inl unsatl  -> 
      (False, map (\n -> Just ((integer_of_nat n), 1)) unsatl)
    Inr mapping ->
      (True,
      map
      (\i ->
        case lookupa mapping (nat_of_integer i) of
          Nothing -> Nothing
          Just r -> Just (n, d)
            where
              (n, d) = (integer_of_int n', integer_of_int d')
              (n', d') = quotient_of r
      )
      [0..3]
      )


optSimplex :: [PolyConstraint] -> Integer -> Opt -> (Bool, [Maybe (Integer, Integer)])
optSimplex pcs varToMaximize minOrMax = simplexHelper (map polyConstraintToConstraint pcs) (nat_of_integer varToMaximize) Nothing
  where
    simplexHelper :: [Constraint] -> Nat -> Maybe (Sum [Nat] (Mapping Nat Rational)) -> (Bool, [Maybe (Integer, Integer)])
    simplexHelper cs' varToMaximize mPreviousSimplexResult =
      case simplexResult of
        Inl unsatl ->
          case mPreviousSimplexResult of
            Just previousSimplexResult -> showSimplexResult previousSimplexResult
            Nothing -> showSimplexResult simplexResult
        Inr mapping ->
          case lookupa mapping varToMaximize of
            Nothing -> trace "Var to maximize not found" $ undefined
            Just r  -> 
              trace (show (both integer_of_int (quotient_of r))) $
              case minOrMax of
                Max -> simplexHelper ((Simplex.GT (LinearPoly (Fmap_of_list [(varToMaximize, 1)])) r) : cs') varToMaximize (Just simplexResult)
                Min -> simplexHelper ((Simplex.LT (LinearPoly (Fmap_of_list [(varToMaximize, 1)])) r) : cs') varToMaximize (Just simplexResult)
      where
        simplexResult = simplex cs'
        both f (a, b) = (f a, f b)

-- Perform the two phase simplex method with a given objective function to maximize and a system of constraints
-- assumes objFunction and system is not empty. Returns the a pair with the first item being the variable representing
-- the objective function and the second item being the values of all variables appearing in the system (including the
-- objective function).
twoPhaseSimplex :: VarConstMap -> [PolyConstraint] -> Maybe (Integer, [(Integer, Rational)])
twoPhaseSimplex objFunction system = 
  if null artificialVars
    then Just $ displayResults $ simplexPivot ((objectiveVar, Util.EQ objFunction 0) : systemWithBasicVars)
    else 
      case Data.List.lookup objectiveVar removeArtificialVarsFromPhase1Tableau of
        Nothing -> trace "objective row not found in phase 1 tableau" Nothing
        Just (Util.EQ _ r) ->
          if r == 0 
            then Just $ displayResults $ simplexPivot $ (objectiveVar, newObjFunction) : tail removeArtificialVarsFromPhase1Tableau
            else trace "rhs not zero after phase 1, thus original tableau is infeasible" Nothing
        _ -> trace "objective row is not in EQ form" Nothing

  where
    displayResults :: [(Integer, PolyConstraint)] -> (Integer, [(Integer, Rational)])
    displayResults tableau =
      (
        objectiveVar,
        map (second getRhs) $ filter (\(basicVar,_) -> basicVar `notElem` slackVars ++ artificialVars) tableau
      )

    maxVar =
      Prelude.max
      (maximum $ map 
      (\case
          Util.LT vcm _  -> maximum (map fst vcm)
          Util.GT vcm _  -> maximum (map fst vcm)
          Util.LEQ vcm _ -> maximum (map fst vcm)
          Util.GEQ vcm _ -> maximum (map fst vcm)
          Util.EQ vcm _  -> maximum (map fst vcm)
      ) 
      system)
      (maximum (map fst objFunction)) -- This is not logically needed since if a variable does not appear to the system, 
                                      -- we can set this variable to infinity (since we assume all variables are >=0).
                                      -- But, this is safer.  

    (systemWithSlackVars, slackVars)      = systemInStandardForm system maxVar

    maxVarWithSlackVars = if null slackVars then maxVar else maximum slackVars

    (systemWithBasicVars, artificialVars) = systemWithArtificialVars systemWithSlackVars maxVarWithSlackVars 

    finalMaxVar        = if null artificialVars then maxVarWithSlackVars else maximum artificialVars

    objectiveVar  = finalMaxVar + 1

    phase1Tableau = simplexPivot $ addArtificialObjective systemWithBasicVars artificialVars objectiveVar

    removeArtificialVarsFromPhase1Tableau = 
      map
      (\case
        (basicVar, Util.EQ vcm r) -> (basicVar, Util.EQ (filter (\(var, _) -> var `notElem` artificialVars) vcm) r)
        _ -> undefined
      )
      phase1Tableau

    objFunctionVars = map fst objFunction

    phase1RowsInObjFunction = filter (\(var, _) -> var `elem` objFunctionVars) removeArtificialVarsFromPhase1Tableau

    phase1RowsInObjFunctionWithoutBasicVarsInLHS = 
      map 
      (\case
        (basicVar, Util.EQ vcm r) -> (basicVar, Util.EQ (filter (\(var, _) -> var /= basicVar) vcm) r)
        _ -> undefined
      ) 
      phase1RowsInObjFunction

    newObjFunction =
      foldl1
      addRows
      $ map
        (
          \(var, coeff) ->
            case Data.List.lookup var phase1RowsInObjFunctionWithoutBasicVarsInLHS of
              Nothing -> 
                -- trace ("var " ++ show var ++ " not found") $
                Util.EQ [(var, coeff * (-1))] 0 -- moving var to LHS
              Just row ->
                -- trace (show var ++ ":::" ++ show row)
                mulRow row coeff                -- TODO: think about why we do not negate this coeff. Rows are already in LHS so this is fine?
        )
        objFunction

    -- System in standard form, a tableau using only EQ. Add slack vars where necessary. This may give you
    -- an infeasible system if slack vars are negative. If a constraint is already EQ, set the basic var to Nothing
    -- Final system is a list of equalities for the given system. To be feasible, all vars must be >= 0.
    systemInStandardForm :: [PolyConstraint] -> Integer -> ([(Maybe Integer, PolyConstraint)], [Integer])
    systemInStandardForm pcs maxVar = (finalSystem, finalSlackVars)
      where
        (finalSystem, finalSlackVars) = addSlackVarsToSystem pcs maxVar []

        addSlackVarsToSystem :: [PolyConstraint] -> Integer -> [Integer] -> ([(Maybe Integer, PolyConstraint)], [Integer])
        addSlackVarsToSystem []  _       sVars = ([], sVars)

        addSlackVarsToSystem (Util.EQ v r : xs) maxVar sVars = 
          -- addSlackVarsToSystem (Util.GEQ v r : Util.LEQ v r : xs) maxVar sVars
          ((Nothing, Util.EQ v r) : newSystem, newSlackVars) 
          where
            (newSystem, newSlackVars) = addSlackVarsToSystem xs maxVar sVars
        addSlackVarsToSystem (Util.LEQ v r : xs) maxVar  sVars = ((Just newSlackVar, Util.EQ (v ++ [(newSlackVar, 1)]) r) : newSystem, newSlackVars)
          where
            newSlackVar = maxVar + 1
            (newSystem, newSlackVars) = addSlackVarsToSystem xs newSlackVar (newSlackVar : sVars)
        addSlackVarsToSystem (Util.LT v r : xs) maxVar   sVars = ((Just newSlackVar, Util.EQ (v ++ [(newSlackVar, 1)]) r) : newSystem, newSlackVars) -- TODO: Could add some delta to the coefficient of the slack variable to make this valid? 
          where
            newSlackVar = maxVar + 1
            (newSystem, newSlackVars) = addSlackVarsToSystem xs newSlackVar (newSlackVar : sVars)
        addSlackVarsToSystem (Util.GEQ v r : xs) maxVar  sVars = ((Just newSlackVar, Util.EQ (v ++ [(newSlackVar, -1)]) r) : newSystem, newSlackVars)
          where
            newSlackVar = maxVar + 1
            (newSystem, newSlackVars) = addSlackVarsToSystem xs newSlackVar (newSlackVar : sVars)
        addSlackVarsToSystem (Util.GT v r : xs) maxVar   sVars = ((Just newSlackVar, Util.EQ (v ++ [(newSlackVar, -1)]) r) : newSystem, newSlackVars) -- TODO: Could add some delta to the coefficient of the slack variable to make this valid? 
          where
            newSlackVar = maxVar + 1
            (newSystem, newSlackVars) = addSlackVarsToSystem xs newSlackVar (newSlackVar : sVars)

    -- Add artificial vars to a system.
    -- Artificial vars are added when:
    --  Basic var is Nothing (When the original constraint was already an EQ)
    --  Slack var is equal to a negative value (this is infeasible, all vars need to be >= 0)
    --  Final system will be a feasible artificial system.
    -- We keep track of artificial vars so they can be eliminated once phase 1 is complete
    systemWithArtificialVars :: [(Maybe Integer, PolyConstraint)] -> Integer -> ([(Integer, PolyConstraint)], [Integer])
    systemWithArtificialVars polyConstraints maxVar = addArtificialVarsToSystem polyConstraints maxVar
      where
        addArtificialVarsToSystem :: [(Maybe Integer, PolyConstraint)] -> Integer -> ([(Integer, PolyConstraint)], [Integer])
        addArtificialVarsToSystem [] _                                = ([],[])
        addArtificialVarsToSystem ((mVar, Util.EQ v r) : pcs) maxVar  =
          case mVar of
            Nothing ->
              if r >= 0 
                then ((newArtificialVar, Util.EQ (v ++ [(newArtificialVar, 1)]) r) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
                else 
                  ((newArtificialVar, Util.EQ (invertedMap ++ [(newArtificialVar, 1)]) (r * (-1))) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
                  where
                    invertedMap = map (second (* (-1))) v
            Just basicVar ->
              if r >= 0
                then ((basicVar, Util.EQ v r) : newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
                else ((newArtificialVar, Util.EQ (invertedMap ++ [(newArtificialVar, 1)]) (r * (-1))) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar) 
                  where
                    invertedMap = map (second (* (-1))) v
          where
            newArtificialVar = maxVar + 1
            (newSystemWithNewMaxVar, artificialVarsWithNewMaxVar) = addArtificialVarsToSystem pcs newArtificialVar

            (newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar) = addArtificialVarsToSystem pcs maxVar

    -- Create an artificial objective using the given list of artificialVars and the tableau.
    -- The artificial objective is the negative sum of all artificial vars.
    -- Artificial vars are equal to the row for which they are the basic variable.
    addArtificialObjective :: [(Integer, PolyConstraint)] -> [Integer] -> Integer -> [(Integer, PolyConstraint)]
    addArtificialObjective pcs artificialVars objectiveVar = (objectiveVar, negatedSumWithoutZeroCoeffs) : pcs
      where
        initialObjective = Util.EQ (map (, -1) artificialVars) 0
        rowsToAdd = map snd $ filter (\(i, _) -> i `elem` artificialVars) pcs
        finalSum = Data.List.foldl addRows initialObjective rowsToAdd 
        negatedSum = 
          case finalSum of
            Util.EQ vcm r -> Util.EQ ((objectiveVar, 1) : map (second (* (-1))) vcm) (r * (-1))
            _             -> undefined
        negatedSumWithoutZeroCoeffs = 
          case negatedSum of
            Util.EQ vcm r -> Util.EQ (filter (\(_,c) -> c /= 0) vcm) r


-- Perform the simplex pivot algorithm on a system with artificial vars and the first row being the objective function
simplexPivot :: [(Integer, PolyConstraint)] -> [(Integer, PolyConstraint)]
simplexPivot tableau = dictionaryFormToTableau . simplexPhase2Loop $ tableauInDictionaryForm tableau
  where
    simplexPhase2Loop :: [(Nat, Linear_poly)] -> [(Nat, Linear_poly)]
    simplexPhase2Loop normalizedTableau =
      case mostNegative (head normalizedTableau) of
        Nothing -> normalizedTableau
        Just pivotNonBasicVar -> 
          let
            mPivotBasicVar = ratioTest (tail normalizedTableau) pivotNonBasicVar Nothing Nothing
          in
            case mPivotBasicVar of
              Nothing -> 
                trace ("Ratio test failed on non-basic var: " ++ show pivotNonBasicVar)
                undefined
              Just pivotBasicVar -> 
                -- trace (show pivotNonBasicVar)
                -- trace (show pivotBasicVar)
                -- trace (show invertedLinPoly)
                -- trace (show newTableau)
                simplexPhase2Loop newTableau 
                where
                  newTableau = pivot_tableau_code pivotBasicVar pivotNonBasicVar normalizedTableau

    -- pivotBasicVar    = ratioTest (tail invertedLinPoly) pivotNonBasicVar Nothing Nothing

    ratioTest :: [(Nat, Linear_poly)] -> Nat -> Maybe Nat -> Maybe Rational -> Maybe Nat
    ratioTest []                    _               mCurrentMinBasicVar _           = mCurrentMinBasicVar
    ratioTest ((basicVar, lp) : xs) mostNegativeVar mCurrentMinBasicVar mCurrentMin =
      case Prelude.lookup mostNegativeVar lp' of
        Nothing                         -> ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin
        Just currentCoeff ->
          case Prelude.lookup (Nat (-1)) lp' of
            Nothing  -> 
              undefined -- Shouldn't happen
            Just rhs ->
              if currentCoeff >= 0 || rhs > 0 
                then 
                  -- trace (show currentCoeff)
                  ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin -- Both coeffs need to be negative. rhs is allowed to be zero
                else 
                  case mCurrentMin of
                    Nothing         -> ratioTest xs mostNegativeVar (Just basicVar) (Just (rhs / currentCoeff))
                    Just currentMin ->
                      if (rhs / currentCoeff) <= currentMin
                        then ratioTest xs mostNegativeVar (Just basicVar) (Just (rhs / currentCoeff))
                        else ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin

      where
        Fmap_of_list lp' = linear_poly_map lp

    mostNegative :: (Nat, Linear_poly) -> Maybe Nat
    mostNegative (_, lp) = 
      if largestCoeff <= 0 
        then Nothing
        else Just largestVar

      where
        (largestVar, largestCoeff) = findLargestCoeff lp' Nothing

        Fmap_of_list lp' = linear_poly_map lp

        findLargestCoeff :: [(Nat, Rational)] -> Maybe (Nat, Rational) -> (Nat, Rational)
        findLargestCoeff [] mCurrentMax                  = 
          case mCurrentMax of
            Just currentMax -> currentMax
            _ -> undefined
        findLargestCoeff ((var, coeff) : xs) mCurrentMax = 
          if var == (Nat (-1)) 
            then findLargestCoeff xs mCurrentMax
            else 
              case mCurrentMax of
                Nothing         -> findLargestCoeff xs (Just (var, coeff))
                Just currentMax ->
                  if snd currentMax >= coeff 
                    then findLargestCoeff xs mCurrentMax
                    else findLargestCoeff xs (Just (var, coeff))

    -- Converts a Tableau to dictionary form.
    -- In dictionary form, every variable apart from the basic variable in a row
    -- is in the LHS, each row is equal to the basic variable which is 
    -- defined in the first  element of the pair.
    -- We use (Nat -1) to represent a rational constant (The RHS in the tableau).
    tableauInDictionaryForm :: [(Integer, PolyConstraint)] -> [(Nat, Linear_poly)]
    tableauInDictionaryForm []                      = []
    tableauInDictionaryForm ((basicVar, pc) : pcs)  =
      case pc of
        Util.EQ vcm r ->  (Nat basicVar, LinearPoly (Fmap_of_list ((Nat (-1), r * (-1)) : map (\(var, coeff) -> (Nat var, coeff * (-1))) (filter (\(v,_) -> v /= basicVar) vcm)))) : tableauInDictionaryForm pcs

    -- Converts a list of rows in dictionary form to a tableau
    dictionaryFormToTableau :: [(Nat, Linear_poly)] -> [(Integer, PolyConstraint)]
    dictionaryFormToTableau [] = []
    dictionaryFormToTableau ((Nat basicVar, LinearPoly (Fmap_of_list varMap)) : rows) = 
        (basicVar, Util.EQ ((basicVar, 1) : map (\(v,c) -> (integer_of_nat v, c * (-1))) varMap') (r * (-1))) : dictionaryFormToTableau rows
      where
        r = 
          case Data.List.lookup (Nat (-1)) varMap of
            Just r -> r
            Nothing -> trace "RHS not found in dictionary, setting to zero" 0

        varMap' = filter (\(v,_) -> v /= Nat (-1)) varMap

mulRow :: PolyConstraint -> Rational -> PolyConstraint
mulRow pc c =
  case pc of
    Util.EQ vcm r -> Util.EQ (map (second (*c)) vcm) (c * r)
    _ -> undefined

addRows :: PolyConstraint -> PolyConstraint -> PolyConstraint
addRows pc1 pc2 = 
  case pc1 of
    Util.EQ vcm1 r1 ->
      case pc2 of
        Util.EQ vcm2 r2 ->
          Util.EQ (addVarMapList sortedVarMaps) (r1 + r2)
          where
            sortedVarMaps = sort $ vcm1 ++ vcm2

            addVarMapList :: [(Integer, Rational)] -> [(Integer, Rational)]
            addVarMapList []                          = []
            addVarMapList [(v, c)]                    = [(v, c)]
            addVarMapList ((v1, c1) : (v2, c2) : vcm) =
              if v1 == v2
                then addVarMapList $ (v1, c1 + c2) : vcm
                else (v1, c1) : addVarMapList ((v2, c2) : vcm)
        _ -> undefined
    _ -> undefined

-- removeBasicVarsFromOtherRows :: [(Integer, PolyConstraint)] -> [(Integer, PolyConstraint)]
-- removeBasicVarsFromOtherRows pcs = addAllRowsToTableau pcs pcs
--   where
--     addAllRowsToTableau :: [(Integer, PolyConstraint)] -> [(Integer, PolyConstraint)] -> [(Integer, PolyConstraint)]
--     addAllRowsToTableau []                           lastTableau = lastTableau
--     addAllRowsToTableau (currentRow : remainingRows) lastTableau = addAllRowsToTableau remainingRows $ addGivenRowToRestOfTableau currentRow lastTableau

--     addGivenRowToRestOfTableau :: (Integer, PolyConstraint) -> [(Integer, PolyConstraint)] -> [(Integer, PolyConstraint)]
--     addGivenRowToRestOfTableau (basicVar1, pc1) originalPcs =
--       map 
--       (\(basicVar2, pc2) ->
--         if basicVar1 == basicVar2
--           then (basicVar2, pc2)
--           else 
--             case pc2 of
--               Util.EQ vcm2 _ ->
--                 case Prelude.lookup basicVar1 vcm2 of
--                   Nothing -> (basicVar2, pc2)
--                   Just c2 -> 
--                     case pc1 of
--                       Util.EQ vcm1 _ ->
--                         case Prelude.lookup basicVar1 vcm1 of
--                           Just c1 ->
--                             (basicVar2, addRows (mulRow pc1 (negate (c2 / c1))) pc2)
--                           Nothing -> trace "basic variable does not exist in it's own row" undefined
--                       _ -> trace "pc1 is not Util.EQ" undefined
--               _ -> trace "pc2 is not Util.EQ" undefined
                
--       ) 
--       originalPcs

-- setAllBasicVarCoeffsToOne :: [(Integer, PolyConstraint)] -> [(Integer, PolyConstraint)]
-- setAllBasicVarCoeffsToOne []              = []
-- setAllBasicVarCoeffsToOne ((i, pc) : pcs) = newRow : setAllBasicVarCoeffsToOne pcs
--   where
--     newRow = 
--       case pc of
--         Util.EQ vcm _ ->
--           case Prelude.lookup i vcm of
--             Just c -> (i, mulRow pc (1/c))
--             Nothing -> trace "basic variable does not exist in it's own row" undefined

-- removeVarsEqualTo :: [(Integer, PolyConstraint)] -> Rational -> [(Integer, PolyConstraint)]
-- removeVarsEqualTo [] _ = []
-- removeVarsEqualTo ((i, pc) : pcs) x =
--   case pc of
--     Util.EQ vcm r -> (i, Util.EQ (filter (\(_, c) -> c /= x) vcm) r) : removeVarsEqualTo pcs x
--     _ -> undefined

-- removeBasicVars :: [(Integer, PolyConstraint)] -> [(Integer, PolyConstraint)]
-- removeBasicVars ((i, pc) : pcs) =
--   case pc of
--     Util.EQ vcm r -> (i, Util.EQ (filter (\(var, _) -> var /= i) vcm) r) : removeBasicVars pcs 
