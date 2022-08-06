module Hex.Stage.Render.Impl.DocInstruction.Encode where

import Hex.Common.Box qualified as Box
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hex.Stage.Render.Interface.DocInstruction (DocInstruction (..), RuleSpan (..))
import Hex.Stage.Render.Interface.DocInstruction qualified as DocInstruction
import Hexlude

hBoxElemToDocInstructions :: BoxElem.HBoxElem -> [DocInstruction]
hBoxElemToDocInstructions = \case
  BoxElem.HBoxHBaseElem (BoxElem.ElemCharacter c) ->
    [AddCharacter c.unCharacter.contents]
  BoxElem.HVBoxElem e ->
    vBoxElemToDocInstructions Horizontal e

vBoxElemToDocInstructions :: Axis -> BoxElem.VBoxElem -> [DocInstruction]
vBoxElemToDocInstructions ax = \case
  BoxElem.VBoxBaseElem (BoxElem.ElemFontDefinition fontDefinition) ->
    pure (DefineFont fontDefinition)
  BoxElem.VBoxBaseElem (BoxElem.ElemFontSelection fontNr) ->
    pure (SelectFont fontNr)
  BoxElem.VBoxBaseElem (BoxElem.ElemBox baseBox) ->
    boxContentsToDocInstructions baseBox <> [Move ax $ Box.boxSpanAlongAxis ax baseBox.unBaseBox]
  BoxElem.VBoxBaseElem (BoxElem.ElemKern kern) ->
    pure (Move ax $ kern.unKern)
  BoxElem.BoxGlue g ->
    pure (Move ax $ g.sgDimen)

boxContentsToDocInstructions :: BoxElem.BaseBox -> [DocInstruction]
boxContentsToDocInstructions baseBox =
  let contentsInstrs = case baseBox.unBaseBox.contents of
        BoxElem.HBoxContents hBoxElemSeq -> foldMap hBoxElemToDocInstructions hBoxElemSeq.unHBoxElemSeq
        BoxElem.VBoxContents vBoxElemSeq -> foldMap (vBoxElemToDocInstructions Vertical) vBoxElemSeq.unVBoxElemSeq
        BoxElem.RuleContents ->
          let vSpan = Box.boxSpanAlongAxis Vertical baseBox.unBaseBox
              hSpan = Box.boxSpanAlongAxis Horizontal baseBox.unBaseBox
           in [AddRule $ RuleSpan {vSpan, hSpan}]
   in [PushStack] <> contentsInstrs <> [PopStack]

pageToDocInstructions :: Page.Page -> [DocInstruction]
pageToDocInstructions (Page.Page vBoxElems) =
  [BeginNewPage] <> foldMap (vBoxElemToDocInstructions Vertical) vBoxElems.unVBoxElemSeq <> [EndPage]

pagesToDocInstructions :: [Page.Page] -> [DocInstruction]
pagesToDocInstructions pages =
  let rawInstrs = foldMap pageToDocInstructions pages

      usedFontNrs = flip mapMaybe rawInstrs $ \case
        SelectFont fNr -> Just fNr
        _ -> Nothing
   in flip filter rawInstrs $ \case
        DefineFont fontDef -> fontDef.fontNr `elem` usedFontNrs
        _ -> True
