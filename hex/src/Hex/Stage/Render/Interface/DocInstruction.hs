module Hex.Stage.Render.Interface.DocInstruction where

import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.Quantity qualified as Q
import Hexlude

data DocInstruction
  = BeginNewPage
  | EndPage
  | DefineFont FontDefinition
  | SelectFont FontNumber
  | AddCharacter Code.CharCode
  | AddRule RuleSpan
  | Move Axis Q.Length
  | PushStack
  | PopStack
  -- \| DoSpecial Text
  deriving stock (Show, Generic)

data RuleSpan = RuleSpan {vSpan, hSpan :: Q.Length}
  deriving stock (Show, Generic)

fmtDocInstruction :: Fmt DocInstruction
fmtDocInstruction = F.later $ \case
  BeginNewPage ->
    "BeginNewPage"
  EndPage ->
    "EndPage"
  DefineFont fontDefinition ->
    F.bformat ("DefineFont " |%| fmtFontDefinition) fontDefinition
  SelectFont fontNumber ->
    F.bformat ("SelectFont " |%| fmtFontNumber) fontNumber
  AddCharacter charBox ->
    F.bformat ("AddCharacter " |%| Code.fmtCharCode) charBox
  AddRule rule ->
    F.bformat ("AddRule " |%| F.shown) rule
  Move Vertical len ->
    F.bformat ("Move Down " |%| Q.fmtLengthWithUnit) len
  Move Horizontal len ->
    F.bformat ("Move Right " |%| Q.fmtLengthWithUnit) len
  PushStack ->
    "PushStack"
  PopStack ->
    "PopStack"

fmtDocInstructions :: Fmt [DocInstruction]
fmtDocInstructions = F.unlined fmtDocInstruction

data FontDefinition = FontDefinition
  { fontDefChecksum :: Word32,
    fontDefDesignSize :: Q.Length,
    fontDefDesignScale :: Q.Length,
    fontPath :: HexFilePath,
    fontName :: Text,
    fontNr :: FontNumber
  }
  deriving stock (Show, Generic)

fmtFontDefinition :: Fmt FontDefinition
fmtFontDefinition = "Font " |%| F.accessed (.fontName) (F.squoted F.stext)

newtype FontNumber = FontNumber {unFontNumber :: Q.HexInt}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

fmtFontNumber :: Fmt FontNumber
fmtFontNumber = "FontNumber " |%| F.accessed (.unFontNumber) Q.fmtHexInt
