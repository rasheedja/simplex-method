{- |
Module      : Linear.Simplex.Types
Description : Custom types
Copyright   : (c) Junaid Rasheed, 2020-2022
License     : BSD-3
Maintainer  : jrasheed178@gmail.com
Stability   : experimental
-}
module Linear.Simplex.Types where

{- | List of 'Integer' variables with their 'Rational' coefficients.
 There is an implicit addition between elements in this list.
 Users must only provide positive integer variables.

 Example: [(2, 3), (6, (-1), (2, 1))] is equivalent to 3x2 + (-x6) + x2.
-}
type VarConstMap = [(Integer, Rational)]

{- | For specifying constraints in a system.
 The LHS is a 'VarConstMap', and the RHS, is a 'Rational' number.
 LEQ [(1, 2), (2, 1)] 3.5 is equivalent to 2x1 + x2 <= 3.5.
 Users must only provide positive integer variables.

 Example: LEQ [(2, 3), (6, (-1), (2, 1))] 12.3 is equivalent to 3x2 + (-x6) + x2 <= 12.3.
-}
data PolyConstraint
  = LEQ VarConstMap Rational
  | GEQ VarConstMap Rational
  | EQ VarConstMap Rational
  deriving (Show, Eq)

{- | Create an objective function.
 We can either 'Max'imize or 'Min'imize a 'VarConstMap'.
-}
data ObjectiveFunction = Max VarConstMap | Min VarConstMap deriving (Show, Eq)

{- | A 'Tableau' of equations.
 Each pair in the list is a row.
 The first item in the pair specifies which 'Integer' variable is basic in the equation.
 The second item in the pair is an equation.
 The 'VarConstMap' in the second equation is a list of variables with their coefficients.
 The RHS of the equation is a 'Rational' constant.
-}
type Tableau = [(Integer, (VarConstMap, Rational))]

{- | Type representing equations.
 Each pair in the list is one equation.
 The first item of the pair is the basic variable, and is on the LHS of the equation with a coefficient of one.
 The RHS is represented using a `VarConstMap`.
 The integer variable -1 is used to represent a 'Rational' on the RHS
-}
type DictionaryForm = [(Integer, VarConstMap)]
