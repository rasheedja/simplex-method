-- |
-- Module      : Linear.Expr.Types
-- Description : Util functions for terms
-- Copyright   : (c) Junaid Rasheed, 2020-2024
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
module Linear.Term.Util where

import qualified Data.List as L
import Linear.Term.Types
  ( Term (..)
  , TermVarsOnly (..)
  )
import Linear.Var.Types (Var)

-- TODO: Test each function when reasonable
simplifyTerm :: Term -> Term
simplifyTerm (CoeffTerm 0 _) = ConstTerm 0
simplifyTerm (CoeffTerm 1 v) = VarTerm v
simplifyTerm t = t

negateTerm :: Term -> Term
negateTerm (ConstTerm c) = ConstTerm (-c)
negateTerm (CoeffTerm (-1) v) = VarTerm v
negateTerm (CoeffTerm c v) = CoeffTerm (-c) v
negateTerm (VarTerm v) = CoeffTerm (-1) v

-- Function to get all vars from a Term
termVar :: Term -> Maybe Var
termVar (VarTerm v) = Just v
termVar (CoeffTerm _ v) = Just v
termVar _ = Nothing

zeroConstTerm :: Term -> Term
zeroConstTerm (ConstTerm _) = ConstTerm 0
zeroConstTerm t = t

isConstTerm :: Term -> Bool
isConstTerm (ConstTerm _) = True
isConstTerm _ = False

-- | Normalize a list of 'Term's where each term is added together.
normalizeTerms :: [Term] -> [Term]
normalizeTerms =
  L.sort
    . map simplifyTerm
    . combineTerms
    . L.sortBy orderForCombineTerms
    . map (varToCoeff . simplifyTerm)
  where
    orderForCombineTerms :: Term -> Term -> Ordering
    orderForCombineTerms _ (VarTerm _) = error "Unexpected VarTerm in orderForCombineTerms"
    orderForCombineTerms (VarTerm _) _ = error "Unexpected VarTerm in orderForCombineTerms"
    orderForCombineTerms (ConstTerm c1) (ConstTerm c2) = compare c1 c2
    orderForCombineTerms (CoeffTerm c1 v1) (CoeffTerm c2 v2) =
      case compare v1 v2 of
        EQ -> compare c1 c2
        x -> x
    orderForCombineTerms (ConstTerm _) (CoeffTerm _ _) = LT
    orderForCombineTerms (CoeffTerm _ _) (ConstTerm _) = GT

    varToCoeff :: Term -> Term
    varToCoeff (VarTerm v) = CoeffTerm 1 v
    varToCoeff t = t

    combineTerms :: [Term] -> [Term]
    combineTerms [] = []
    combineTerms [ConstTerm 0] = []
    combineTerms [CoeffTerm 0 _] = []
    combineTerms [x] = [x]
    combineTerms allXs@(x1 : x2 : xs) =
      case (x1, x2) of
        (ConstTerm 0, _) -> combineTerms (x2 : xs)
        (_, ConstTerm 0) -> combineTerms (x1 : xs)
        (CoeffTerm 0 _, _) -> combineTerms (x2 : xs)
        (_, CoeffTerm 0 _) -> combineTerms (x1 : xs)
        (ConstTerm c1, ConstTerm c2) ->
          if c1 + c2 == 0
            then combineTerms xs
            else combineTerms (ConstTerm (c1 + c2) : xs)
        (CoeffTerm c1 v1, CoeffTerm c2 v2) ->
          if v1 == v2
            then combineTerms (CoeffTerm (c1 + c2) v1 : xs)
            else x1 : combineTerms (x2 : xs)
        _otherwise -> x1 : combineTerms (x2 : xs)

normalizeTermsVarsOnly :: [TermVarsOnly] -> [TermVarsOnly]
normalizeTermsVarsOnly = map unsafeTermToTermVarsOnly . normalizeTerms . map termVarsOnlyToTerm

termToTermVarsOnly :: Term -> Either String TermVarsOnly
termToTermVarsOnly (VarTerm v) = Right $ VarTermVO v
termToTermVarsOnly (CoeffTerm c v) = Right $ CoeffTermVO c v
termToTermVarsOnly (ConstTerm _) = Left "termToTermVarsOnly: ConstTerm not allowed"

unsafeTermToTermVarsOnly :: Term -> TermVarsOnly
unsafeTermToTermVarsOnly t =
  case termToTermVarsOnly t of
    Right x -> x
    Left e -> error e

termVarsOnlyToTerm :: TermVarsOnly -> Term
termVarsOnlyToTerm (VarTermVO v) = VarTerm v
termVarsOnlyToTerm (CoeffTermVO c v) = CoeffTerm c v
