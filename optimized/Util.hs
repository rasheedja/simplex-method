{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Util where

import qualified Simplex as S;
import Prelude hiding (EQ);
import Data.List
import Data.Bifunctor

import Debug.Trace (trace)

-- data Opt = Max | Min

type VarConstMap = [(Integer, Rational)]

data PolyConstraint =
  LEQ VarConstMap Rational      | 
  GEQ VarConstMap Rational      | 
  EQ VarConstMap Rational       deriving (Show, Eq);
  
data ObjectiveFunction = Max VarConstMap | Min VarConstMap

rhs :: PolyConstraint -> Rational
rhs (LEQ _ r) = r
rhs (GEQ _ r) = r
rhs (EQ _ r) = r

lhs :: PolyConstraint -> VarConstMap
lhs (LEQ vcm _) = vcm
lhs (GEQ vcm _) = vcm
lhs (EQ vcm _) = vcm

updateRhs :: PolyConstraint -> Rational -> PolyConstraint
updateRhs (EQ vcm _) r = EQ vcm r
updateRhs (LEQ vcm _) r = LEQ vcm r
updateRhs (GEQ vcm _) r = GEQ vcm r

updateLhs :: PolyConstraint -> VarConstMap -> PolyConstraint
updateLhs (EQ _ r) vcm = EQ vcm r 
updateLhs (LEQ _ r) vcm = LEQ vcm r 
updateLhs (GEQ _ r) vcm = GEQ vcm r 

updatePc :: PolyConstraint -> VarConstMap -> Rational -> PolyConstraint
updatePc (EQ _ _) = EQ
updatePc (LEQ _ _) = LEQ
updatePc (GEQ _ _) = GEQ

filterOutVars :: PolyConstraint -> [Integer] -> PolyConstraint
filterOutVars (Util.EQ vcm r) vars = Util.EQ (filter (\(v, _) -> v `notElem` vars) vcm) r
filterOutVars (Util.LEQ vcm r) vars = Util.LEQ (filter (\(v, _) -> v `notElem` vars) vcm) r
filterOutVars (Util.GEQ vcm r) vars = Util.GEQ (filter (\(v, _) -> v `notElem` vars) vcm) r

filterInVars :: PolyConstraint -> [Integer] -> PolyConstraint
filterInVars (Util.EQ vcm r) vars = Util.EQ (filter (\(v, _) -> v `elem` vars) vcm) r
filterInVars (Util.LEQ vcm r) vars = Util.LEQ (filter (\(v, _) -> v `elem` vars) vcm) r
filterInVars (Util.GEQ vcm r) vars = Util.GEQ (filter (\(v, _) -> v `elem` vars) vcm) r

-- Add a sorted list of VarConstMaps, folding where the variables are equal
addVarConstMap :: [(Integer, Rational)] -> [(Integer, Rational)]
addVarConstMap []                          = []
addVarConstMap [(v, c)]                    = [(v, c)]
addVarConstMap ((v1, c1) : (v2, c2) : vcm) =
  if v1 == v2
    then addVarConstMap $ (v1, c1 + c2) : vcm
    else (v1, c1) : addVarConstMap ((v2, c2) : vcm)

varConstMapToLinearPoly :: VarConstMap -> S.Linear_poly
varConstMapToLinearPoly vcm = S.LinearPoly (S.Fmap_of_list (map (first S.nat_of_integer) vcm))

polyConstraintToConstraint :: PolyConstraint -> S.Constraint
polyConstraintToConstraint pc =
  case pc of
    LEQ vcm r -> S.LEQ (varConstMapToLinearPoly vcm) r
    GEQ vcm r -> S.GEQ (varConstMapToLinearPoly vcm) r
    EQ vcm r -> S.EQ (varConstMapToLinearPoly vcm) r

showSimplexResult :: S.Sum [S.Nat] (S.Mapping S.Nat Rational) -> (Bool, [Maybe (Integer, Integer)])
showSimplexResult result =
  case result of
    S.Inl unsatl  -> 
      (False, map (\n -> Just (S.integer_of_nat n, 1)) unsatl)
    S.Inr mapping ->
      (True,
      map
      (\i ->
        case S.lookupa mapping (S.nat_of_integer i) of
          Nothing -> Nothing
          Just r -> Just (n, d)
            where
              (n, d) = (S.integer_of_int n', S.integer_of_int d')
              (n', d') = S.quotient_of r
      )
      [0..3]
      )


-- optSimplex :: [PolyConstraint] -> Integer -> Opt -> (Bool, [Maybe (Integer, Integer)])
-- optSimplex pcs varToMaximize minOrMax = simplexHelper (map polyConstraintToConstraint pcs) (nat_of_integer varToMaximize) Nothing
--   where
--     simplexHelper :: [Constraint] -> Nat -> Maybe (Sum [Nat] (Mapping Nat Rational)) -> (Bool, [Maybe (Integer, Integer)])
--     simplexHelper cs' varToMaximize mPreviousSimplexResult =
--       case simplexResult of
--         Inl unsatl ->
--           case mPreviousSimplexResult of
--             Just previousSimplexResult -> showSimplexResult previousSimplexResult
--             Nothing -> showSimplexResult simplexResult
--         Inr mapping ->
--           case lookupa mapping varToMaximize of
--             Nothing -> trace "Var to maximize not found" $ undefined
--             Just r  -> 
--               trace (show (both integer_of_int (quotient_of r))) $
--               case minOrMax of
--                 Max -> simplexHelper ((Simplex.GT (LinearPoly (Fmap_of_list [(varToMaximize, 1)])) r) : cs') varToMaximize (Just simplexResult)
--                 Min -> simplexHelper ((Simplex.LT (LinearPoly (Fmap_of_list [(varToMaximize, 1)])) r) : cs') varToMaximize (Just simplexResult)
--       where
--         simplexResult = simplex cs'
--         both f (a, b) = (f a, f b)

-- Perform the two phase simplex method with a given objective function to maximize and a system of constraints
-- assumes objFunction and system is not empty. Returns the a pair with the first item being the variable representing
-- the objective function and the second item being the values of all variables appearing in the system (including the
-- objective function).
twoPhaseSimplex :: ObjectiveFunction -> [PolyConstraint] -> Maybe (Integer, [(Integer, Rational)])
twoPhaseSimplex objFunction system = 
  if null artificialVars
    then displayResults <$> simplexPivot ((objectiveVar, EQ objectiveRow 0) : systemWithBasicVars)
    else 
      case lookup objectiveVar removeArtificialVarsFromPhase1Tableau of
        Nothing -> trace "objective row not found in phase 1 tableau" Nothing
        Just (EQ _ r) ->
          if r == 0 
            then fmap displayResults $ simplexPivot $ (objectiveVar, newObjFunction) : tail removeArtificialVarsFromPhase1Tableau
            else trace "rhs not zero after phase 1, thus original tableau is infeasible" Nothing
        _ -> trace "objective row is not in EQ form" Nothing

  where
    objectiveRow =
      case objFunction of
        Max objective -> map (second negate) objective
        Min objective -> objective -- Turning Min into Max, negation cancels out

    displayResults :: [(Integer, PolyConstraint)] -> (Integer, [(Integer, Rational)])
    displayResults tableau =
      (
        objectiveVar,
        case objFunction of
          Max _ -> 
            map 
            (second rhs) 
            $ filter (\(basicVar,_) -> basicVar `notElem` slackVars ++ artificialVars) tableau
          Min _ -> 
            map -- We maximized -objVar, so we negate the objVar to get the final value
            (\(basicVar, pc) -> if basicVar == objectiveVar then (basicVar, negate (rhs pc)) else (basicVar, rhs pc))
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
      (maximum (map fst objectiveRow)) -- This is not logically needed since if a variable does not appear to the system, 
                                      -- we can set this variable to infinity (since we assume all variables are >=0).
                                      -- But, this is safer.  

    (systemWithSlackVars, slackVars)      = systemInStandardForm system maxVar []

    maxVarWithSlackVars = if null slackVars then maxVar else maximum slackVars

    (systemWithBasicVars, artificialVars) = systemWithArtificialVars systemWithSlackVars maxVarWithSlackVars 

    finalMaxVar        = if null artificialVars then maxVarWithSlackVars else maximum artificialVars

    objectiveVar  = finalMaxVar + 1

    phase1Tableau = simplexPivot $ addArtificialObjective systemWithBasicVars artificialVars objectiveVar

    removeArtificialVarsFromPhase1Tableau = 
      map
      (\(basicVar, pc) -> (basicVar, filterOutVars pc artificialVars))
      (S.the phase1Tableau) -- FIXME: Unsafe

    objFunctionVars = map fst objectiveRow

    phase1RowsInObjFunction = filter (\(var, _) -> var `elem` objFunctionVars) removeArtificialVarsFromPhase1Tableau

    phase1RowsInObjFunctionWithoutBasicVarsInLHS = 
      map 
      (\(basicVar, pc) -> (basicVar, filterOutVars pc [basicVar])) 
      phase1RowsInObjFunction

    newObjFunction =
      foldl1
      addRows
      $ map
        (
          \(var, coeff) ->
            case lookup var phase1RowsInObjFunctionWithoutBasicVarsInLHS of
              Nothing -> 
                -- trace ("var " ++ show var ++ " not found") $
                EQ [(var, coeff * (-1))] 0 -- moving var to LHS
              Just row ->
                -- trace (show var ++ ":::" ++ show row)
                mulRow row coeff                -- TODO: think about why we do not negate this coeff. Rows are already in LHS so this is fine?
        )
        objectiveRow

    -- System in standard form, a tableau using only EQ. Add slack vars where necessary. This may give you
    -- an infeasible system if slack vars are negative. If a constraint is already EQ, set the basic var to Nothing
    -- Final system is a list of equalities for the given system. To be feasible, all vars must be >= 0.
    systemInStandardForm :: [PolyConstraint] -> Integer -> [Integer] -> ([(Maybe Integer, PolyConstraint)], [Integer])
    systemInStandardForm []  _       sVars = ([], sVars)
    systemInStandardForm (EQ v r : xs) maxVar sVars = ((Nothing, EQ v r) : newSystem, newSlackVars) 
      where
        (newSystem, newSlackVars) = systemInStandardForm xs maxVar sVars
    systemInStandardForm (LEQ v r : xs) maxVar  sVars = ((Just newSlackVar, EQ (v ++ [(newSlackVar, 1)]) r) : newSystem, newSlackVars)
      where
        newSlackVar = maxVar + 1
        (newSystem, newSlackVars) = systemInStandardForm xs newSlackVar (newSlackVar : sVars)
    systemInStandardForm (GEQ v r : xs) maxVar  sVars = ((Just newSlackVar, EQ (v ++ [(newSlackVar, 1)]) r) : newSystem, newSlackVars)
      where
        newSlackVar = maxVar + 1
        (newSystem, newSlackVars) = systemInStandardForm xs newSlackVar (newSlackVar : sVars)

    -- Add artificial vars to a system.
    -- Artificial vars are added when:
    --  Basic var is Nothing (When the original constraint was already an EQ)
    --  Slack var is equal to a negative value (this is infeasible, all vars need to be >= 0)
    --  Final system will be a feasible artificial system.
    -- We keep track of artificial vars so they can be eliminated once phase 1 is complete
    systemWithArtificialVars :: [(Maybe Integer, PolyConstraint)] -> Integer -> ([(Integer, PolyConstraint)], [Integer])
    systemWithArtificialVars [] _                                = ([],[])
    systemWithArtificialVars ((mVar, EQ v r) : pcs) maxVar  =
      case mVar of
        Nothing ->
          if r >= 0 
            then ((newArtificialVar, EQ (v ++ [(newArtificialVar, 1)]) r) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
            else 
              ((newArtificialVar, EQ (invertedMap ++ [(newArtificialVar, 1)]) (r * (-1))) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar)
              where
                invertedMap = map (second (* (-1))) v
        Just basicVar ->
          if r >= 0
            then ((basicVar, EQ v r) : newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar)
            else ((newArtificialVar, EQ (invertedMap ++ [(newArtificialVar, 1)]) (r * (-1))) : newSystemWithNewMaxVar, newArtificialVar : artificialVarsWithNewMaxVar) 
              where
                invertedMap = map (second (* (-1))) v
      where
        newArtificialVar = maxVar + 1
        (newSystemWithNewMaxVar, artificialVarsWithNewMaxVar) = systemWithArtificialVars pcs newArtificialVar

        (newSystemWithoutNewMaxVar, artificialVarsWithoutNewMaxVar) = systemWithArtificialVars pcs maxVar

    -- Create an artificial objective using the given list of artificialVars and the tableau.
    -- The artificial objective is the negative sum of all artificial vars.
    -- Artificial vars are equal to the row for which they are the basic variable.
    addArtificialObjective :: [(Integer, PolyConstraint)] -> [Integer] -> Integer -> [(Integer, PolyConstraint)]
    addArtificialObjective pcs artificialVars objectiveVar = (objectiveVar, negatedSumWithoutZeroCoeffs) : pcs
      where
        initialObjective = EQ (map (, -1) artificialVars) 0
        rowsToAdd = map snd $ filter (\(i, _) -> i `elem` artificialVars) pcs
        finalSum = foldl addRows initialObjective rowsToAdd 
        negatedSum = 
          case finalSum of
            pc -> updatePc pc ((objectiveVar, 1) : map (second (* (-1))) (lhs pc)) (rhs pc * (-1))
            -- EQ vcm r -> EQ ((objectiveVar, 1) : map (second (* (-1))) vcm) (r * (-1))
        negatedSumWithoutZeroCoeffs = 
          case negatedSum of
            pc -> updateLhs pc (filter (\(_, c) -> c /= 0) (lhs pc))
            -- EQ vcm r -> EQ (filter (\(_,c) -> c /= 0) vcm) r

-- Perform the simplex pivot algorithm on a system with artificial vars and the first row being the objective function
simplexPivot :: [(Integer, PolyConstraint)] -> Maybe [(Integer, PolyConstraint)]
simplexPivot tableau = 
  case simplexPhase2Loop $ tableauInDictionaryForm tableau of
    Just resultsInDictForm -> Just $ dictionaryFormToTableau resultsInDictForm  
    Nothing -> Nothing
  where
    simplexPhase2Loop :: [(S.Nat, S.Linear_poly)] -> Maybe [(S.Nat, S.Linear_poly)]
    simplexPhase2Loop normalizedTableau =
      case mostPositive (head normalizedTableau) of
        Nothing -> Just normalizedTableau
        Just pivotNonBasicVar -> 
          let
            mPivotBasicVar = ratioTest (tail normalizedTableau) pivotNonBasicVar Nothing Nothing
          in
            case mPivotBasicVar of
              Nothing -> trace ("Ratio test failed on non-basic var: " ++ show pivotNonBasicVar) Nothing
              Just pivotBasicVar -> 
                -- trace (show pivotNonBasicVar)
                -- trace (show pivotBasicVar)
                -- trace (show invertedLinPoly)
                -- trace (show newTableau)
                simplexPhase2Loop newTableau 
                where
                  newTableau = S.pivot_tableau_code pivotBasicVar pivotNonBasicVar normalizedTableau

    -- pivotBasicVar    = ratioTest (tail invertedLinPoly) pivotNonBasicVar Nothing Nothing

    ratioTest :: [(S.Nat, S.Linear_poly)] -> S.Nat -> Maybe S.Nat -> Maybe Rational -> Maybe S.Nat
    ratioTest []                    _               mCurrentMinBasicVar _           = mCurrentMinBasicVar
    ratioTest ((basicVar, lp) : xs) mostNegativeVar mCurrentMinBasicVar mCurrentMin =
      case lookup mostNegativeVar lp' of
        Nothing                         -> ratioTest xs mostNegativeVar mCurrentMinBasicVar mCurrentMin
        Just currentCoeff ->
          case lookup (S.Nat (-1)) lp' of
            Nothing  -> trace "RHS not found in row in dict form" Nothing
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
        S.Fmap_of_list lp' = S.linear_poly_map lp

    mostPositive :: (S.Nat, S.Linear_poly) -> Maybe S.Nat
    mostPositive (_, lp) = 
      case findLargestCoeff lp' Nothing of
        Just (largestVar, largestCoeff) ->
          if largestCoeff <= 0 
            then Nothing
            else Just largestVar
        Nothing -> trace "No variables in first row when looking for most positive" Nothing

      where
        S.Fmap_of_list lp' = S.linear_poly_map lp

        findLargestCoeff :: [(S.Nat, Rational)] -> Maybe (S.Nat, Rational) -> Maybe (S.Nat, Rational)
        findLargestCoeff [] mCurrentMax                  = mCurrentMax
        findLargestCoeff ((var, coeff) : xs) mCurrentMax = 
          if var == S.Nat (-1) 
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
    tableauInDictionaryForm :: [(Integer, PolyConstraint)] -> [(S.Nat, S.Linear_poly)]
    tableauInDictionaryForm []                      = []
    tableauInDictionaryForm ((basicVar, pc) : pcs)  =
      case pc of
        EQ vcm r ->  (S.Nat basicVar, S.LinearPoly (S.Fmap_of_list ((S.Nat (-1), r * (-1)) : map (\(var, coeff) -> (S.Nat var, coeff * (-1))) (filter (\(v,_) -> v /= basicVar) vcm)))) : tableauInDictionaryForm pcs

    -- Converts a list of rows in dictionary form to a tableau
    dictionaryFormToTableau :: [(S.Nat, S.Linear_poly)] -> [(Integer, PolyConstraint)]
    dictionaryFormToTableau [] = []
    dictionaryFormToTableau ((S.Nat basicVar, S.LinearPoly (S.Fmap_of_list varMap)) : rows) = 
        (basicVar, EQ ((basicVar, 1) : map (\(v,c) -> (S.integer_of_nat v, c * (-1))) varMap') (r * (-1))) : dictionaryFormToTableau rows
      where
        r = 
          case lookup (S.Nat (-1)) varMap of
            Just r -> r
            Nothing -> trace "RHS not found in dictionary, setting to zero" 0

        varMap' = filter (\(v,_) -> v /= S.Nat (-1)) varMap

mulRow :: PolyConstraint -> Rational -> PolyConstraint
mulRow pc c = updatePc pc (map (second (*c)) (lhs pc)) (rhs pc * c)

addRows :: PolyConstraint -> PolyConstraint -> PolyConstraint
addRows (Util.EQ vcm1 r1) (Util.EQ vcm2 r2)   = Util.EQ (addVarConstMap (sort (vcm1 ++ vcm2))) (r1 + r2)
addRows (Util.LEQ vcm1 r1) (Util.LEQ vcm2 r2) = Util.LEQ (addVarConstMap (sort (vcm1 ++ vcm2))) (r1 + r2)
addRows (Util.GEQ vcm1 r1) (Util.GEQ vcm2 r2) = Util.GEQ (addVarConstMap (sort (vcm1 ++ vcm2))) (r1 + r2)
addRows _ _                                   = undefined -- FIXME: Unsafe. Can only add rows with matching data constructors
                                                          -- Leaving it like this is easier for folding

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
