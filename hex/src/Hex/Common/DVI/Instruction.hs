module Hex.Common.DVI.Instruction where

import Formatting qualified as F
import Hex.Common.Box qualified as Box
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.Quantity qualified as Q
import Hexlude

data DVIInstruction
  = BeginNewPage
  | DefineFont FontDefinition
  | SelectFont HSt.Font.FontNumber
  | AddCharacter Box.CharBox
  | AddRule Box.Rule
  | Move Axis Q.Length
  | PushStack
  | PopStack
  -- \| DoSpecial Text
  deriving stock (Show, Generic)

fmtDviInstruction :: Fmt DVIInstruction
fmtDviInstruction = F.later $ \case
  BeginNewPage ->
    "BeginNewPage"
  DefineFont fontDefinition ->
    F.bformat ("DefineFont " |%| fmtFontDefinition) fontDefinition
  SelectFont fontNumber ->
    F.bformat ("SelectFont " |%| HSt.Font.fmtFontNumber) fontNumber
  AddCharacter charBox ->
    F.bformat ("AddCharacter " |%| Box.fmtCharBox) charBox
  AddRule rule ->
    F.bformat ("AddRule " |%| Box.fmtRule) rule
  Move Vertical len ->
    F.bformat ("Move Down " |%| Q.fmtLengthWithUnit) len
  Move Horizontal len ->
    F.bformat ("Move Right " |%| Q.fmtLengthWithUnit) len
  PushStack ->
    "PushStack"
  PopStack ->
    "PopStack"

data FontDefinition = FontDefinition
  { fontDefChecksum :: Word32,
    fontDefDesignSize :: Q.Length,
    fontDefDesignScale :: Q.Length,
    fontPath :: HexFilePath,
    fontName :: Text,
    fontNr :: HSt.Font.FontNumber
  }
  deriving stock (Show, Generic)

fmtFontDefinition :: Fmt FontDefinition
fmtFontDefinition = "Font " |%| F.accessed (.fontName) (F.squoted F.stext)
