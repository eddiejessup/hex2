module Hex.Common.HexState.Interface.Font where

import Formatting qualified as F
import Hex.Common.Font qualified as Font
import Hex.Common.Quantity qualified as Q
import Hexlude

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

data FontDefinition = FontDefinition
  { fontDefChecksum :: Word32,
    fontDefDesignSize :: Q.Length,
    fontDefDesignScale :: Q.Length,
    fontPath :: HexFilePath,
    fontName :: Text,
    fontNr :: Font.FontNumber
  }
  deriving stock (Show, Generic)

fmtFontDefinition :: Fmt FontDefinition
fmtFontDefinition = "Font " |%| F.accessed (.fontName) (F.squoted F.stext)

data CharacterAttrs = CharacterAttrs
  { width, height, depth, italicCorrection :: Q.Length
  }
  deriving stock (Show, Generic)
