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
          putStrLn "The following test failed: \n"
          putStrLn ("Objective Function (Non-prettified): " ++ show testObjective)
          putStrLn ("Constraints        (Non-prettified): " ++ show testConstraints)
          putStrLn "====================================\n"
          putStrLn ("Objective Function (Prettified): " ++ prettyShowObjectiveFunction testObjective)
          putStrLn "Constraints        (Prettified): "
          putStrLn (concatMap ((\c -> "\t" ++ prettyShowPolyConstraint c ++ "\n")) testConstraints)
          putStrLn "====================================\n"
          putStrLn ("Expected Solution      (Full): " ++ show expectedResult)
          putStrLn ("Actual Solution        (Full): " ++ show testResult)
          putStrLn ("Expected Solution (Objective): " ++ show (extractObjectiveValue expectedResult))
          putStrLn ("Actual Solution   (Objective): " ++ show (extractObjectiveValue testResult))
