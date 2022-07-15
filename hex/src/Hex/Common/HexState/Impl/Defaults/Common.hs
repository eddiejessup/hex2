module Hex.Common.HexState.Impl.Defaults.Common where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hexlude

initialiseFiniteMap :: (Enum a, Ord a, Bounded a) => (a -> v) -> Map a v
initialiseFiniteMap keyToVal =
  Map.fromSet keyToVal (Set.fromList [minBound .. maxBound])
