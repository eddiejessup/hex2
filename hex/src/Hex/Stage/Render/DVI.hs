module Hex.Stage.Render.DVI where

import Formatting qualified as F
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as B
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hexlude

data DVIInstruction
  = BeginNewPage
  | DefineFont B.FontDefinition
  | SelectFont HSt.Font.FontNumber
  | AddCharacter B.CharBox
  | AddRule B.Rule
  | Move Q.Axis Q.Length
  | PushStack
  | PopStack
  -- \| DoSpecial Text
  deriving stock (Show, Generic)

fmtDviInstruction :: Fmt DVIInstruction
fmtDviInstruction = F.later $ \case
  BeginNewPage ->
    "BeginNewPage"
  DefineFont fontDefinition ->
    F.bformat ("DefineFont " |%| B.fmtFontDefinition) fontDefinition
  SelectFont fontNumber ->
    F.bformat ("SelectFont " |%| HSt.Font.fmtFontNumber) fontNumber
  AddCharacter charBox ->
    F.bformat ("AddCharacter " |%| B.fmtCharBox) charBox
  AddRule rule ->
    F.bformat ("AddRule " |%| B.fmtRule) rule
  Move Q.Vertical len ->
    F.bformat ("Move Down " |%| Q.fmtLengthWithUnit) len
  Move Q.Horizontal len ->
    F.bformat ("Move Right " |%| Q.fmtLengthWithUnit) len
  PushStack ->
    "PushStack"
  PopStack ->
    "PopStack"

hBoxElemToDVI :: B.HBoxElem -> [DVIInstruction]
hBoxElemToDVI = \case
  B.HBoxHBaseElem (B.ElemCharacter c) ->
    [AddCharacter c]
  B.HVBoxElem e ->
    vBoxElemToDVI Q.Horizontal e

vBoxElemToDVI :: Q.Axis -> B.VBoxElem -> [DVIInstruction]
vBoxElemToDVI ax = \case
  B.VBoxBaseElem (B.ElemFontDefinition fontDefinition) ->
    pure (DefineFont fontDefinition)
  B.VBoxBaseElem (B.ElemFontSelection fontNr) ->
    pure (SelectFont fontNr)
  B.VBoxBaseElem (B.ElemBox box) ->
    boxContentsToDVI box.contents <> [Move ax $ B.boxSpanAlongAxis ax box]
  B.VBoxBaseElem (B.ElemKern kern) ->
    pure (Move ax $ kern.unKern)
  B.BoxGlue g ->
    pure (Move ax $ g.sgDimen)

boxContentsToDVI :: B.BaseBoxContents -> [DVIInstruction]
boxContentsToDVI contents =
  [PushStack] <> contentsToDVI contents <> [PopStack]
  where
    contentsToDVI :: B.BaseBoxContents -> [DVIInstruction]
    contentsToDVI = \case
      B.HBoxContents hBoxElemSeq -> foldMap hBoxElemToDVI hBoxElemSeq.unHBoxElemSeq
      B.VBoxContents vBoxElemSeq -> foldMap (vBoxElemToDVI Q.Vertical) vBoxElemSeq.unVBoxElemSeq
      B.RuleContents -> []

pageToDVI :: Page.Page -> [DVIInstruction]
pageToDVI (Page.Page vBoxElems) =
  [BeginNewPage] <> foldMap (vBoxElemToDVI Q.Vertical) vBoxElems.unVBoxElemSeq

pagesToDVI :: Seq Page.Page -> [DVIInstruction]
pagesToDVI pages = foldMap pageToDVI pages
