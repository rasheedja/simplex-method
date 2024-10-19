-- |
-- Module: Linear.Expr.Util
-- Description: Utility functions for linear expressions
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: jrasheed178@gmail.com
-- Stability: experimental
module Linear.Expr.Util where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Linear.Expr.Types (Expr (..), ExprVarsOnly (..))
import Linear.Term.Types (Term (..), TermVarsOnly (..))
import Linear.Term.Util
  ( negateTerm
  , normalizeTerms
  , normalizeTermsVarsOnly
  , simplifyTerm
  , termVarsOnlyToTerm
  , unsafeTermToTermVarsOnly
  , zeroConstTerm
  )
import Linear.Var.Types (SimplexNum, Var)

-- | Convert an 'Expr' to a list of 'Term's.
exprToList :: Expr -> [Term]
exprToList = unExpr

exprVarsOnlyToList :: ExprVarsOnly -> [TermVarsOnly]
exprVarsOnlyToList = unExprVarsOnly

-- | Convert a list of 'Term's to an 'Expr'.
listToExpr :: [Term] -> Expr
listToExpr = Expr

listToExprVarsOnly :: [TermVarsOnly] -> ExprVarsOnly
listToExprVarsOnly = ExprVarsOnly

exprVars :: Expr -> Set.Set Var
exprVars = Set.fromList . Maybe.mapMaybe termVars . exprToList
  where
    termVars :: Term -> Maybe Var
    termVars (ConstTerm _) = Nothing
    termVars (CoeffTerm _ v) = Just v
    termVars (VarTerm v) = Just v

exprVarsOnlyVars :: ExprVarsOnly -> Set.Set Var
exprVarsOnlyVars = exprVars . exprVarsOnlyToExpr

exprVarsOnlyMaxVar :: ExprVarsOnly -> Var
exprVarsOnlyMaxVar = maximum . exprVarsOnlyVars

simplifyExpr :: Expr -> Expr
simplifyExpr = listToExpr . normalizeTerms . exprToList

simplifyExprVarsOnly :: ExprVarsOnly -> ExprVarsOnly
simplifyExprVarsOnly = listToExprVarsOnly . normalizeTermsVarsOnly . exprVarsOnlyToList

sumExprConstTerms :: Expr -> SimplexNum
sumExprConstTerms (Expr ts) = sumExprConstTerms ts
  where
    sumExprConstTerms = sum . Maybe.mapMaybe termConst

    termConst :: Term -> Maybe SimplexNum
    termConst (ConstTerm c) = Just c
    termConst _ = Nothing

zeroConstExpr :: Expr -> Expr
zeroConstExpr (Expr ts) = Expr $ map zeroConstTerm ts

negateExpr :: Expr -> Expr
negateExpr (Expr ts) = Expr $ map negateTerm ts

addExpr :: Expr -> Expr -> Expr
addExpr e1 e2 =
  -- Safe as Expr :+ Term is the only constructor
  simplifyExpr . listToExpr $ (exprToList e1 <> exprToList e2)

subtractExpr :: Expr -> Expr -> Expr
subtractExpr e1 e2 = addExpr e1 (negateExpr e2)

substVarExpr :: Var -> Expr -> Expr -> Expr
substVarExpr var varReplacement = simplifyExpr . listToExpr . aux . exprToList
  where
    replacementTerms = exprToList varReplacement

    aux :: [Term] -> [Term]
    aux [] = []
    aux (t : ts) = case t of
      (VarTerm tV) -> if tV == var then aux ts ++ replacementTerms else t : aux ts
      (CoeffTerm tC tV) ->
        if tV == var
          then
            let newReplacementTerms =
                  map
                    ( simplifyTerm
                        . \case
                          (CoeffTerm rC rV) -> CoeffTerm (tC * rC) rV
                          (VarTerm rV) -> CoeffTerm tC rV
                          (ConstTerm rC) -> ConstTerm (tC * rC)
                    )
                    replacementTerms
            in  aux ts ++ newReplacementTerms
          else t : aux ts
      (ConstTerm _) -> t : aux ts

substVarExprVarsOnly :: Var -> ExprVarsOnly -> ExprVarsOnly -> ExprVarsOnly
substVarExprVarsOnly var varReplacement expr =
  let varReplacement' = exprVarsOnlyToExpr varReplacement
      expr' = exprVarsOnlyToExpr expr
      result' = substVarExpr var varReplacement' expr'
  in  unsafeExprToExprVarsOnly result'

unsafeExprToExprVarsOnly :: Expr -> ExprVarsOnly
unsafeExprToExprVarsOnly (Expr ts) = ExprVarsOnly (map unsafeTermToTermVarsOnly ts)

exprToExprVarsOnly :: Expr -> Either String ExprVarsOnly
exprToExprVarsOnly expr@(Expr ts) = do
  if any isConstTerm ts
    then
      if sumExprConstTerms expr == 0
        then Right $ ExprVarsOnly []
        else Left $ "safeExprToExprVarsOnly: Expr contains ConstTerm. Expr: " <> show expr
    else Right $ unsafeExprToExprVarsOnly expr
  where
    isConstTerm :: Term -> Bool
    isConstTerm (ConstTerm _) = True
    isConstTerm _ = False

exprVarsOnlyToExpr :: ExprVarsOnly -> Expr
exprVarsOnlyToExpr (ExprVarsOnly ts) = Expr $ map termVarsOnlyToTerm ts
