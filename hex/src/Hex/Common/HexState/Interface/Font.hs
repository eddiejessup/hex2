module Hex.Common.HexState.Interface.Font where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hexlude

newtype FontNumber = FontNumber {fontNumber :: Q.HexInt}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

fmtFontNumber :: Fmt FontNumber
fmtFontNumber = "FontNumber " |%| F.accessed (.fontNumber) Q.fmtHexInt

data FontSpecialChar
  = HyphenChar -- \hyphenchar
  | SkewChar -- \skewchar
  deriving stock (Show, Eq, Generic)

data FontRange
  = TextSizeFontRange -- \textfont
  | ScriptSizeFontRange -- \scriptfont
  | ScriptScriptSizeFontRange -- \scriptscriptfont
  deriving stock (Show, Eq, Ord, Generic)

fmtFontRange :: Fmt FontRange
fmtFontRange = F.later $ \case
  TextSizeFontRange -> "textfont"
  ScriptSizeFontRange -> "scriptfont"
  ScriptScriptSizeFontRange -> "scriptscriptfont"

data FamilyMember = FamilyMember {fontRange :: FontRange, familyNumber :: Q.HexInt}
  deriving stock (Show, Eq, Ord, Generic)

fmtFamilyMember :: Fmt FamilyMember
fmtFamilyMember = F.accessed (.fontRange) fmtFontRange |%| " of family " <> F.accessed (.familyNumber) Q.fmtHexInt
