module Hex.Common.HexState.Impl.Defaults.Common where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hexlude

initialiseFiniteMap :: Ord a => [a] -> (a -> v) -> Map a v
initialiseFiniteMap vals keyToVal =
  Map.fromSet keyToVal (Set.fromList vals)
