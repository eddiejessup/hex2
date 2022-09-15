module Hex.Stage.Render.Interface.DocInstruction where

import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.Font qualified as Font
import Hex.Common.Quantity qualified as Q
import Hexlude

data DocInstruction
  = BeginNewPage
  | EndPage
  | SelectFont Font.FontNumber
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
  SelectFont fontNumber ->
    F.bformat ("SelectFont " |%| Font.fmtFontNumber) fontNumber
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
