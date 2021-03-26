module Hex.Interpret.Build.List.Horizontal.Badness where

import Hex.Quantity qualified as H.Q
import Hexlude

-- Badness.

newtype Badness = Badness_ {unBadness :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

zeroBadness :: Badness
zeroBadness = Badness_ 0

infBadness :: Badness
infBadness = Badness_ H.Q.tenK

finiteFlexBadness :: H.Q.Length -> H.Q.Length -> Badness
finiteFlexBadness requiredFlex flexibility
  | flexibility == H.Q.zeroLength = infBadness
  | otherwise =
    let r = H.Q.lengthRatio requiredFlex flexibility
     in min infBadness $ Badness_ $ round $ (abs r ^ (3 :: Int)) * 100
