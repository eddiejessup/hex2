module Hex.Stage.Render.Impl.DocInstruction.Encode where

import Hex.Common.Box qualified as Box
import Hex.Common.Font qualified as Font
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hex.Stage.Render.Interface.DocInstruction (DocInstruction (..), RuleSpan (..))
import Hexlude

hBoxElemToDocInstructions :: State (Maybe Font.FontNumber) :> es => BoxElem.HBoxElem -> Eff es [DocInstruction]
hBoxElemToDocInstructions = \case
  BoxElem.HBoxHBaseElem (BoxElem.ElemCharacter c) -> do
    let thisFontNr = c.charBoxFont

        addCharInstr = AddCharacter c.unCharBox.contents
        selectFontInstr = SelectFont thisFontNr

    get >>= \case
      Just curFontnr
        | curFontnr == thisFontNr ->
            pure [addCharInstr]
      _ -> do
        put (Just thisFontNr)
        pure [selectFontInstr, addCharInstr]
  BoxElem.HVBoxElem e ->
    vBoxElemToDocInstructions Horizontal e

vBoxElemToDocInstructions :: State (Maybe Font.FontNumber) :> es => Axis -> BoxElem.VBoxElem -> Eff es [DocInstruction]
vBoxElemToDocInstructions ax = \case
  BoxElem.VBoxBaseElem (BoxElem.ElemBox baseBox) -> do
    boxInstrs <- boxContentsToDocInstructions baseBox
    pure $ boxInstrs <> [Move ax $ Box.boxSpanAlongAxis ax baseBox.unBaseBox]
  BoxElem.VBoxBaseElem (BoxElem.ElemKern kern) ->
    pure [Move ax $ kern.unKern]

boxContentsToDocInstructions :: State (Maybe Font.FontNumber) :> es => BoxElem.BaseBox -> Eff es [DocInstruction]
boxContentsToDocInstructions baseBox = do
  contentsInstrs <- case baseBox.unBaseBox.contents of
    BoxElem.HBoxContents hBoxElemSeq -> foldMapM hBoxElemToDocInstructions hBoxElemSeq.unHBoxElemSeq
    BoxElem.VBoxContents vBoxElemSeq -> foldMapM (vBoxElemToDocInstructions Vertical) vBoxElemSeq.unVBoxElemSeq
    BoxElem.RuleContents ->
      let vSpan = Box.boxSpanAlongAxis Vertical baseBox.unBaseBox
          hSpan = Box.boxSpanAlongAxis Horizontal baseBox.unBaseBox
       in pure [AddRule $ RuleSpan {vSpan, hSpan}]
  pure $ [PushStack] <> contentsInstrs <> [PopStack]

pageToDocInstructions :: State (Maybe Font.FontNumber) :> es => Page.Page -> Eff es [DocInstruction]
pageToDocInstructions (Page.Page vBoxElems) = do
  coreInstrs <- foldMapM (vBoxElemToDocInstructions Vertical) vBoxElems.unVBoxElemSeq
  pure $ [BeginNewPage] <> coreInstrs <> [EndPage]

pagesToDocInstructions_ :: State (Maybe Font.FontNumber) :> es => [Page.Page] -> Eff es [DocInstruction]
pagesToDocInstructions_ pages = foldMapM pageToDocInstructions pages

pagesToDocInstructions :: [Page.Page] -> ([DocInstruction], [Font.FontNumber])
pagesToDocInstructions pages =
  let rawInstrs = runPureEff $ evalStateLocal @(Maybe Font.FontNumber) Nothing (pagesToDocInstructions_ pages)

      usedFontNrs = flip mapMaybe rawInstrs $ \case
        SelectFont fNr -> Just fNr
        _ -> Nothing
   in (rawInstrs, usedFontNrs)
