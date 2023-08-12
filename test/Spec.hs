module Main where

import Linear.Simplex.Prettify
import Linear.Simplex.Simplex
import Linear.Simplex.Util
import TestFunctions

main :: IO ()
main = runTests testsList

runTests [] = putStrLn "All tests passed"
runTests (((testObjective, testConstraints), expectedResult) : tests) =
  let testResult = twoPhaseSimplex testObjective testConstraints
  in  if testResult == expectedResult
        then runTests tests
        else do
          let msg = "\nThe following test failed: "
                  <> ("\nObjective Function (Non-prettified): " ++ show testObjective)
                  <> ("\nConstraints        (Non-prettified): " ++ show testConstraints)
                  <> "\n===================================="
                  <> ("\nObjective Function (Prettified): " ++ prettyShowObjectiveFunction testObjective)
                  <> "\nConstraints        (Prettified): "
                  <> "\n" <> concatMap (\c -> "\t" ++ prettyShowPolyConstraint c ++ "\n") testConstraints
                  <> "\n===================================="
                  <> ("\nExpected Solution      (Full): " ++ show expectedResult)
                  <> ("\nActual Solution        (Full): " ++ show testResult)
                  <> ("\nExpected Solution (Objective): " ++ show (extractObjectiveValue expectedResult))
                  <> ("\nActual Solution   (Objective): " ++ show (extractObjectiveValue testResult))
                  <> "\n"
          fail msg
