module Hex.Common.Font where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hexlude

newtype FontNumber = FontNumber {unFontNumber :: Q.HexInt}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

fmtFontNumber :: Fmt FontNumber
fmtFontNumber = "FontNumber " |%| F.accessed (.unFontNumber) Q.fmtHexInt
