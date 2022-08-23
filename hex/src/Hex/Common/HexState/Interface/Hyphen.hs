module Hex.Common.HexState.Interface.Hyphen where

import ASCII.Decimal qualified as ASCII
import Data.Text qualified as Tx
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hexlude

data HyphenationPattern
  = HyphenationPattern (NonEmpty (Word8, Code.CharCode)) Word8
  deriving stock (Show, Eq, Generic)

patternAsText :: HyphenationPattern -> Text
patternAsText (HyphenationPattern prePart lastWord) =
  let preAsBS =
        Tx.concat $
          toList prePart <&> \(w, c) ->
            renderWeight w <> renderCharWord c
   in preAsBS <> renderWeight lastWord
  where
    renderWeight w = case w of
      0 -> ""
      _ -> ASCII.showIntegral w

    renderCharWord c = case c.unCharCode of
      0 -> "."
      _ -> Tx.singleton $ Code.unsafeCodeAsChar c

fmtHyphenationPattern :: Fmt HyphenationPattern
fmtHyphenationPattern = F.accessed patternAsText F.stext

newtype WordCodes = WordCodes {unCodeWord :: NonEmpty Code.CharCode}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

fmtWordCodes :: Fmt WordCodes
fmtWordCodes = F.accessed (Code.codesAsText . toList . (.unCodeWord)) F.stext

newtype WordHyphenationPoints = WordHyphenationPoints {unHyphenationSequence :: [Int]}
  deriving stock (Show, Eq, Generic)

fmtWordHyphenationPoints :: Fmt WordHyphenationPoints
fmtWordHyphenationPoints = F.accessed (.unHyphenationSequence) (fmtListOneLine F.int)
