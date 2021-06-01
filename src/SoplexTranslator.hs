-- Module to translate a system with an objective function into a String that can be optimized by SoPlex
-- See: https://soplex.zib.de
module SoplexTranslator where

import Simplex
import Data.Ratio
import Data.List
import Data.Scientific
import System.Process
import System.IO.Unsafe
import System.Exit 

soplexPath :: String
soplexPath = "/home/junaid/Research/opt/soplex/soplex-5.0.1/bin/soplex"

translateToLp :: (ObjectiveFunction, [PolyConstraint]) -> String
translateToLp (obj, cons) = 
  objectiveToLpString obj ++ "\n" ++
  "Subject to" ++ "\n" ++ 
  consToLpString cons ++ "\n" ++
  "End"
  where
    objectiveToLpString :: ObjectiveFunction -> String
    objectiveToLpString (Max objective) =
      "Maximize" ++ "\n" ++
      "\t " ++ "cost: " ++ 
      varConstMapToString objective
    objectiveToLpString (Min objective) =
      "Minimize" ++ "\n" ++
      "\t " ++ "cost: " ++ 
      varConstMapToString objective

    consToLpString :: [PolyConstraint] -> String
    consToLpString cons =
      concatMap
      (\(i, pc) ->
        "\t c_" ++ show i ++ ": \n" ++
        case pc of
          Simplex.LEQ vcm r ->  "\t\t" ++ varConstMapToString vcm ++ "\n" ++
                                "\t\t<= " ++ rationalAsDecimal 100 r ++ "\n"
          Simplex.GEQ vcm r ->  "\t\t" ++ varConstMapToString vcm ++ "\n" ++
                                "\t\t>= " ++ rationalAsDecimal 100 r ++ "\n"
          Simplex.EQ vcm r ->   "\t\t" ++ varConstMapToString vcm ++ "\n" ++
                                "\t\t== " ++ rationalAsDecimal 100 r ++ "\n"
      )
      $ zip [1..] cons

-- |Soplex does not appear to support representing rational numbers using a numerator and denominator, so we use this
-- function to represent a rational as a decimal number.
-- From https://stackoverflow.com/questions/30931369/how-to-convert-a-rational-into-a-pretty-string
-- Modified to handle case where denominator is 1 and to prepend + when positive
rationalAsDecimal :: Int -> Rational -> String
rationalAsDecimal len rat = 
  if den == 1
    then if num >= 0 then "+" ++ show num else show num
    else (if num < 0 then "-" else "+") ++ shows d ("." ++ take len (go next))
    where
        (d, next) = abs num `quotRem` den
        num = numerator rat
        den = denominator rat

        go 0 = ""
        go x = let (d, next) = (10 * x) `quotRem` den
               in shows d (go next)

varConstMapToString :: VarConstMap -> String
varConstMapToString =
  concatMap 
  (\(v, c) -> 
    rationalAsDecimal 100 c ++ " x" ++ show v ++ " ")

findObjectiveValueUsingSoplex :: (ObjectiveFunction, [PolyConstraint]) -> IO (Maybe Rational)
findObjectiveValueUsingSoplex system =
  do
    writeFile soplexFile soplexInput
    (exitCode, output, errDetails) <- readProcessWithExitCode soplexPath [soplexFile] []
    case exitCode of
      ExitSuccess   -> return $ parseObjectiveValue output
      ExitFailure _ -> error $ "Error when running SoPlex on generated soplex.lp. Error message: " ++ show errDetails
  where
    soplexInput = translateToLp system
    soplexFile  = "soplex.lp"

parseObjectiveValue :: String -> Maybe Rational
parseObjectiveValue output =
  if "[optimal]" `elem` outputList
    then toRational . (read :: String -> Scientific) <$> findObjectiveValue outputList
    else Nothing
  where
    outputList = words output

    findObjectiveValue :: [String] -> Maybe String
    findObjectiveValue []                                             = Nothing
    findObjectiveValue ("Objective" : "value" : ":" : objectiveValue : _) = Just objectiveValue
    findObjectiveValue (_ : xs)                                       = findObjectiveValue xs
