module Hex.Stage.Render.Impl.DocInstruction.Encode where

import Hex.Common.Box qualified as Box
import Hex.Common.Font qualified as Font
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Vertical.Page.Types qualified as Page
import Hex.Stage.Render.Interface.DocInstruction (DocInstruction (..), RuleSpan (..))
import Hexlude

hBoxElemToDocInstructions :: State (Maybe Font.FontNumber) :> es => BoxElem.HBoxElem -> Eff es [DocInstruction]
hBoxElemToDocInstructions = \case
  BoxElem.HBoxHBaseElem (BoxElem.CharBoxHBaseElem c) -> do
    let thisFontNr = c.boxedContents.charBoxFont

        addCharInstr = AddCharacter c.boxedContents.charBoxCharCode
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
  BoxElem.VBoxBaseElem (BoxElem.AxOrRuleBoxBaseElem boxedBoxlike) -> do
    boxInstrs <- stackScoped <$> boxContentsToDocInstructions boxedBoxlike
    pure $
      case ax of
        Horizontal ->
          boxInstrs
            <> [Move Horizontal boxedBoxlike.boxedDims.boxWidth]
        Vertical ->
          [Move Vertical boxedBoxlike.boxedDims.boxHeight]
            <> boxInstrs
            <> [Move Vertical boxedBoxlike.boxedDims.boxDepth]
  BoxElem.VBoxBaseElem (BoxElem.KernBaseElem kern) ->
    pure [Move ax kern.unKern]

boxContentsToDocInstructions :: State (Maybe Font.FontNumber) :> es => Box.Boxed BoxElem.AxBoxOrRuleContents -> Eff es [DocInstruction]
boxContentsToDocInstructions boxedBoxlike =
  case boxedBoxlike.boxedContents of
    BoxElem.AxBoxOrRuleContentsAx offsettableElems ->
      case offsettableElems.offsetContents of
        BoxElem.AxBoxElemsH hElems -> do
          let offsetMoveOps = case offsettableElems.offset of
                Nothing ->
                  []
                Just (Box.OffsetInDirection dir x) ->
                  let len = case dir of
                        Forward ->
                          x
                        Backward ->
                          invert x
                   in [Move Vertical len]

          coreOps <- foldMapM hBoxElemToDocInstructions hElems
          pure $ offsetMoveOps <> coreOps
        BoxElem.AxBoxElemsV vElems -> do
          let moveOps = case offsettableElems.offset of
                Nothing -> []
                Just (Box.OffsetInDirection _ _) ->
                  notImplemented "OffsetInDirection for vbox"

          coreOps <- foldMapM (vBoxElemToDocInstructions Vertical) vElems
          pure $ moveOps <> coreOps
    BoxElem.AxBoxOrRuleContentsRule ->
      let vSpan = Box.boxSpanAlongAxis Vertical boxedBoxlike.boxedDims
          hSpan = Box.boxSpanAlongAxis Horizontal boxedBoxlike.boxedDims
       in pure [AddRule RuleSpan {vSpan, hSpan}]

stackScoped :: [DocInstruction] -> [DocInstruction]
stackScoped xs = [PushStack] <> xs <> [PopStack]

pageToDocInstructions :: State (Maybe Font.FontNumber) :> es => Page.Page -> Eff es [DocInstruction]
pageToDocInstructions (Page.Page vBoxElems) = do
  coreInstrs <- foldMapM (vBoxElemToDocInstructions Vertical) vBoxElems
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
