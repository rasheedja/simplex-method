-- |
-- Module: Linear.Expr.Util
-- Description: Utility functions for linear expressions
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: jrasheed178@gmail.com
-- Stability: experimental
module Linear.Expr.Util where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Linear.Expr.Types (Expr (..), ExprVarsOnly (..))
import Linear.Term.Types (Term (..))
import Linear.Term.Util
  ( negateTerm
  , normalizeTerms
  , simplifyTerm
  , unsafeTermToTermVarsOnly
  , zeroConstTerm
  )
import Linear.Var.Types (SimplexNum, Var)

-- | Convert an 'Expr' to a list of 'Term's.
exprToList :: Expr -> [Term]
exprToList (Expr t) = NE.toList t

-- | Convert a list of 'Term's to an 'Expr'.
listToExpr :: [Term] -> Expr
listToExpr [] = Expr $ ConstTerm 0 :| [] -- TODO: Maybe throw an error?
listToExpr ts = Expr $ NE.fromList ts

exprVars :: Expr -> Set.Set Var
exprVars = Set.fromList . Maybe.mapMaybe termVars . exprToList
  where
    termVars :: Term -> Maybe Var
    termVars (ConstTerm _) = Nothing
    termVars (CoeffTerm _ v) = Just v
    termVars (VarTerm v) = Just v

simplifyExpr :: Expr -> Expr
simplifyExpr = listToExpr . normalizeTerms . exprToList

sumExprConstTerms :: Expr -> SimplexNum
sumExprConstTerms (Expr ts) = sumExprConstTerms ts
  where
    sumExprConstTerms = sum . Maybe.mapMaybe termConst . NE.toList

    termConst :: Term -> Maybe SimplexNum
    termConst (ConstTerm c) = Just c
    termConst _ = Nothing

zeroConstExpr :: Expr -> Expr
zeroConstExpr (Expr ts) = Expr $ NE.map zeroConstTerm ts

negateExpr :: Expr -> Expr
negateExpr (Expr ts) = Expr $ NE.map negateTerm ts

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

exprToExprVarsOnly :: Expr -> Either String ExprVarsOnly
exprToExprVarsOnly (Expr ts) = do
  if any isConstTerm ts
    then Left "safeExprToExprVarsOnly: Expr contains ConstTerm"
    else Right $ ExprVarsOnly (NE.map unsafeTermToTermVarsOnly ts)
  where
    isConstTerm :: Term -> Bool
    isConstTerm (ConstTerm _) = True
    isConstTerm _ = False
