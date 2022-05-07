module Hex.Common.HexState.Impl.Font where

import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Types qualified as TFM
import Hexlude
import qualified Formatting as F
import qualified Data.Map as Map

data FontInfo = FontInfo {fontMetrics :: TFM.Font, hyphenChar :: Q.HexInt, skewChar :: Q.HexInt}
  deriving stock (Show, Generic)

fmtFontInfo :: Fmt FontInfo
fmtFontInfo =
  ("hyphenChar: " |%| F.accessed (.hyphenChar) Q.fmtHexInt) |%| "\n" <>
  ("skewChar: " |%| F.accessed (.skewChar) Q.fmtHexInt |%| "\n") <>
  ("TFM font specification: " |%| F.accessed (.fontMetrics) TFM.fmtFont)

fmtFontInfos :: Fmt (Map PT.FontNumber FontInfo)
fmtFontInfos = F.accessed Map.toList fmtKVList
  where
    fmtKVList :: Fmt [(PT.FontNumber, FontInfo)]
    fmtKVList = F.unlined fmtKVEntry

    fmtKVEntry :: Fmt (PT.FontNumber, FontInfo)
    fmtKVEntry = F.accessed fst PT.fmtFontNumber |%| ": " <> F.accessed snd fmtFontInfo
