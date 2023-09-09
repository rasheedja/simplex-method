module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import Linear.Simplex.Prettify
import Linear.Simplex.Simplex
import Linear.Simplex.Types
import Linear.Simplex.Util

import TestFunctions

main :: IO ()
main = runStdoutLoggingT $ filterLogger (\_logSource logLevel -> logLevel > LevelInfo) $ runTests testsList

runTests :: (MonadLogger m, MonadFail m, MonadIO m) => [((ObjectiveFunction, [PolyConstraint]), Maybe Result)] -> m ()
runTests [] = do
  liftIO $ putStrLn "All tests passed"
  pure ()
runTests (((testObjective, testConstraints), expectedResult) : tests) =
  do
    testResult <- twoPhaseSimplex testObjective testConstraints
    if testResult == expectedResult
      then runTests tests
      else do
        let msg =
              "\nThe following test failed: "
                <> ("\nObjective Function (Non-prettified): " ++ show testObjective)
                <> ("\nConstraints        (Non-prettified): " ++ show testConstraints)
                <> "\n===================================="
                <> ("\nObjective Function (Prettified): " ++ prettyShowObjectiveFunction testObjective)
                <> "\nConstraints        (Prettified): "
                <> "\n"
                <> concatMap (\c -> "\t" ++ prettyShowPolyConstraint c ++ "\n") testConstraints
                <> "\n===================================="
                <> ("\nExpected Solution      (Full): " ++ show expectedResult)
                <> ("\nActual Solution        (Full): " ++ show testResult)
                <> ("\nExpected Solution (Objective): " ++ show (extractObjectiveValue expectedResult))
                <> ("\nActual Solution   (Objective): " ++ show (extractObjectiveValue testResult))
                <> "\n"
        fail msg
