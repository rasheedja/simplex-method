module Linear.Var.Util where

import qualified Data.Map as M
import Linear.Var.Types (Var(..), Bounds (..), VarBounds)

validateBounds :: VarBounds -> Bool
validateBounds boundsMap = all soundBounds $ M.toList boundsMap
  where
    soundBounds (_, Bounds lowerBound upperBound) =
      case (lowerBound, upperBound) of
        (Just l, Just u) -> l <= u
        (_, _) -> True

nextVar :: Var -> Var
nextVar = Var . succ . unVar

prevVar :: Var -> Var
prevVar = Var . pred . unVar
