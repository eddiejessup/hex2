module Hex.Common.HexState.Impl.Font where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Types qualified as TFM
import Hexlude

data FontInfo = FontInfo {fontMetrics :: TFM.Font, hyphenChar :: Q.HexInt, skewChar :: Q.HexInt}
  deriving stock (Show, Generic)

fmtFontInfo :: Fmt FontInfo
fmtFontInfo =
  ("hyphenChar: " |%| F.accessed (.hyphenChar) Q.fmtHexInt) |%| "\n"
    <> ("skewChar: " |%| F.accessed (.skewChar) Q.fmtHexInt |%| "\n")
    <> ("TFM font specification: " |%| F.accessed (.fontMetrics) TFM.fmtFont)
