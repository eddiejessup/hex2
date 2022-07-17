module Hex.Stage.Render.DVI where

import Hex.Common.Box qualified as Box
import Hex.Common.DVI.DocInstruction (DocInstruction (..), RuleSpan (..))
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hexlude

hBoxElemToDVI :: BoxElem.HBoxElem -> [DocInstruction]
hBoxElemToDVI = \case
  BoxElem.HBoxHBaseElem (BoxElem.ElemCharacter c) ->
    [AddCharacter c.unCharacter.contents]
  BoxElem.HVBoxElem e ->
    vBoxElemToDVI Horizontal e

vBoxElemToDVI :: Axis -> BoxElem.VBoxElem -> [DocInstruction]
vBoxElemToDVI ax = \case
  BoxElem.VBoxBaseElem (BoxElem.ElemFontDefinition fontDefinition) ->
    pure (DefineFont fontDefinition)
  BoxElem.VBoxBaseElem (BoxElem.ElemFontSelection fontNr) ->
    pure (SelectFont fontNr)
  BoxElem.VBoxBaseElem (BoxElem.ElemBox baseBox) ->
    boxContentsToDVI baseBox <> [Move ax $ Box.boxSpanAlongAxis ax baseBox.unBaseBox]
  BoxElem.VBoxBaseElem (BoxElem.ElemKern kern) ->
    pure (Move ax $ kern.unKern)
  BoxElem.BoxGlue g ->
    pure (Move ax $ g.sgDimen)

boxContentsToDVI :: BoxElem.BaseBox -> [DocInstruction]
boxContentsToDVI baseBox =
  let contentsInstrs = case baseBox.unBaseBox.contents of
        BoxElem.HBoxContents hBoxElemSeq -> foldMap hBoxElemToDVI hBoxElemSeq.unHBoxElemSeq
        BoxElem.VBoxContents vBoxElemSeq -> foldMap (vBoxElemToDVI Vertical) vBoxElemSeq.unVBoxElemSeq
        BoxElem.RuleContents ->
          let vSpan = Box.boxSpanAlongAxis Vertical baseBox.unBaseBox
              hSpan = Box.boxSpanAlongAxis Horizontal baseBox.unBaseBox
           in [AddRule $ RuleSpan {vSpan, hSpan}]
   in [PushStack] <> contentsInstrs <> [PopStack]

pageToDVI :: Page.Page -> [DocInstruction]
pageToDVI (Page.Page vBoxElems) =
  [BeginNewPage] <> foldMap (vBoxElemToDVI Vertical) vBoxElems.unVBoxElemSeq <> [EndPage]

pagesToDVI :: [Page.Page] -> [DocInstruction]
pagesToDVI pages =
  foldMap pageToDVI pages
