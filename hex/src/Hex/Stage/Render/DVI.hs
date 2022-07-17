module Hex.Stage.Render.DVI where

import Hex.Common.Box qualified as Box
import Hex.Common.DVI.Instruction (DVIInstruction (..))
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hexlude

hBoxElemToDVI :: BoxElem.HBoxElem -> [DVIInstruction]
hBoxElemToDVI = \case
  BoxElem.HBoxHBaseElem (BoxElem.ElemCharacter c) ->
    [AddCharacter c]
  BoxElem.HVBoxElem e ->
    vBoxElemToDVI Horizontal e

vBoxElemToDVI :: Axis -> BoxElem.VBoxElem -> [DVIInstruction]
vBoxElemToDVI ax = \case
  BoxElem.VBoxBaseElem (BoxElem.ElemFontDefinition fontDefinition) ->
    pure (DefineFont fontDefinition)
  BoxElem.VBoxBaseElem (BoxElem.ElemFontSelection fontNr) ->
    pure (SelectFont fontNr)
  BoxElem.VBoxBaseElem (BoxElem.ElemBox baseBox) ->
    boxContentsToDVI baseBox.unBaseBox.contents <> [Move ax $ Box.boxSpanAlongAxis ax baseBox.unBaseBox]
  BoxElem.VBoxBaseElem (BoxElem.ElemKern kern) ->
    pure (Move ax $ kern.unKern)
  BoxElem.BoxGlue g ->
    pure (Move ax $ g.sgDimen)

boxContentsToDVI :: BoxElem.BaseBoxContents -> [DVIInstruction]
boxContentsToDVI contents =
  [PushStack] <> contentsToDVI contents <> [PopStack]
  where
    contentsToDVI :: BoxElem.BaseBoxContents -> [DVIInstruction]
    contentsToDVI = \case
      BoxElem.HBoxContents hBoxElemSeq -> foldMap hBoxElemToDVI hBoxElemSeq.unHBoxElemSeq
      BoxElem.VBoxContents vBoxElemSeq -> foldMap (vBoxElemToDVI Vertical) vBoxElemSeq.unVBoxElemSeq
      BoxElem.RuleContents -> []

pageToDVI :: Page.Page -> [DVIInstruction]
pageToDVI (Page.Page vBoxElems) =
  [BeginNewPage] <> foldMap (vBoxElemToDVI Vertical) vBoxElems.unVBoxElemSeq

pagesToDVI :: Seq Page.Page -> [DVIInstruction]
pagesToDVI pages = foldMap pageToDVI pages
