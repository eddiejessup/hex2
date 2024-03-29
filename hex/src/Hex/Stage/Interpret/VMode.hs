module Hex.Stage.Interpret.VMode where

import Formatting qualified as F
import Hex.Capability.Log.Interface (HexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.HexIO.Interface qualified as HIO
import Hex.Common.HexIO.Interface.CharSourceStack (CharSourceStack)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Mode qualified as HSt.Mode
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Horizontal.Paragraph.Break.MultiPass qualified as Break
import Hex.Stage.Build.Horizontal.Set qualified as List.H
import Hex.Stage.Build.ListBuilder.Interface qualified as Build
import Hex.Stage.Build.ListElem (HListElem)
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.AllMode qualified as AllMode
import Hexlude

data VModeCommandResult
  = ContinueVMode
  | EndVMode

handleCommandInVMode ::
  forall es.
  ( HexLog :> es,
    HSt.EHexState :> es,
    HIO.HexIO :> es,
    Error AllMode.InterpretError :> es,
    Build.HexListBuilder :> es,
    ListExtractor.ExtractList :> es
  ) =>
  CharSourceStack ->
  HSt.Mode.ModeVariant ->
  Eval.Command ->
  Eff es VModeCommandResult
handleCommandInVMode oldSrc modeVariant = \case
  Eval.VModeCommand vModeCommand -> case vModeCommand of
    Eval.End ->
      pure EndVMode
    Eval.AddVGlue g -> do
      Build.addVListElement $ ListElem.ListGlue g
      pure ContinueVMode
    Eval.Dump ->
      notImplemented "handleCommandInVMode: Dump"
    Eval.EnterHMode ->
      notImplemented "handleCommandInVMode: EnterHMode"
    Eval.AddVLeaders _leadersSpec ->
      notImplemented "handleCommandInVMode: AddVLeaders"
    Eval.AddVRule rule -> do
      Build.addVListElement $
        ListElem.VListBaseElem $
          BoxElem.AxOrRuleBoxBaseElem $
            Box.Boxed {boxedContents = BoxElem.AxBoxOrRuleContentsRule, boxedDims = rule}
      pure ContinueVMode
    Eval.AddUnwrappedFetchedVBox _fetchedBoxRef ->
      notImplemented "handleCommandInVMode: AddUnwrappedFetchedVBox"
    Eval.AddHAlignedMaterial _boxSpec ->
      notImplemented "VMode: AddHAlignedMaterial"
  Eval.HModeCommand _ ->
    addPara ListExtractor.Indent True
  Eval.ModeDependentCommand modeDependentCommand -> case modeDependentCommand of
    Eval.StartParagraph indentFlag ->
      addPara indentFlag False
    -- \par does nothing in vertical mode.
    Eval.EndParagraph ->
      pure ContinueVMode
    -- <space token> has no effect in vertical modes.
    Eval.AddSpace ->
      pure ContinueVMode
    Eval.ShowToken _lexToken -> notImplemented "VMode: ShowToken"
    Eval.ShowBox _n -> notImplemented "VMode: ShowBox"
    Eval.ShowLists -> notImplemented "VMode: ShowLists"
    Eval.ShowTheInternalQuantity _internalQuantity -> notImplemented "VMode: ShowTheInternalQuantity"
    Eval.ShipOut _box -> notImplemented "VMode: ShipOut"
    Eval.AddMark _text -> notImplemented "VMode: AddMark"
    Eval.AddInsertion _n -> notImplemented "VMode: AddInsertion"
    Eval.AddAdjustment -> notImplemented "VMode: AddAdjustment"
  Eval.ModeIndependentCommand modeIndependentCommand ->
    AllMode.handleModeIndependentCommand modeIndependentCommand >>= \case
      AllMode.SawEndBox ->
        case modeVariant of
          HSt.Mode.OuterModeVariant ->
            throwError AllMode.SawEndBoxInMainVMode
          HSt.Mode.InnerModeVariant ->
            pure EndVMode
      AllMode.DidNotSeeEndBox ->
        pure ContinueVMode
  where
    addPara :: ListExtractor.IndentFlag -> Bool -> Eff es VModeCommandResult
    addPara indentFlag resetSrc = do
      case modeVariant of
        HSt.Mode.OuterModeVariant -> do
          -- Just before switching to horizontal mode to begin scanning a paragraph,
          -- TEX inserts the glue specified by \parskip into the vertical list that
          -- will contain the paragraph, unless that vertical list is empty so far.
          parSkipGlue <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.ParSkip)
          Log.infoLog $ "addPara: Adding \\glue(\\parSkip): " <> F.sformat Q.fmtGlue parSkipGlue
          Build.addVListElement $ ListElem.ListGlue parSkipGlue
        HSt.Mode.InnerModeVariant ->
          pure ()

      -- If the command shifts to horizontal mode, run '\indent', and re-read
      -- the stream as if the command hadn't been read.
      when resetSrc (HIO.putInput oldSrc)

      (endParaReason, paraHList) <- ListExtractor.extractParagraphList indentFlag
      extendVListWithParagraphStateT paraHList
      case endParaReason of
        ListExtractor.EndHListSawEndParaCommand ->
          pure ContinueVMode
        ListExtractor.EndHListSawLeaveBox ->
          throwError AllMode.SawEndBoxInMainVModePara

extendVListWithParagraphStateT ::
  (HSt.EHexState :> es, Log.HexLog :> es, Build.HexListBuilder :> es) =>
  Seq HListElem ->
  Eff es ()
extendVListWithParagraphStateT paraHList = do
  lineBoxes <- setAndBreakHListToHBoxes paraHList
  for_ lineBoxes $ \b ->
    Build.addVListElement $
      ListElem.VListBaseElem $
        BoxElem.AxOrRuleBoxBaseElem $
          b <&> \elems ->
            BoxElem.AxBoxOrRuleContentsAx $
              Box.Offsettable
                { offset = Nothing,
                  offsetContents = BoxElem.AxBoxElemsH elems
                }

setAndBreakHListToHBoxes ::
  (HSt.EHexState :> es, Log.HexLog :> es) =>
  Seq HListElem ->
  Eff es (Seq (Box.Boxed (Seq BoxElem.HBoxElem)))
setAndBreakHListToHBoxes hList = do
  lineHLists <- Break.breakHListMultiPass hList
  hSize <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.HSize)
  for lineHLists $ \lineHList -> do
    let listWidth = ListElem.hListNaturalWidth lineHList
        (hBoxElems, flexSpec) = List.H.setList lineHList hSize

        -- TODO: Implement proper interline glue.
        boxHeight = BoxElem.hBoxNaturalHeight hBoxElems
        boxDepth = BoxElem.hBoxNaturalDepth hBoxElems
        -- TODO: Is this correct?
        boxWidth = hSize
        boxedDims = Box.BoxDims {boxWidth, boxHeight, boxDepth}
    Log.infoLog $
      F.sformat
        ( "setAndBreakHListToHBoxes: setting at page width: "
            |%| Q.fmtLengthWithUnit
            |%| ", list natural-width: "
            |%| Q.fmtLengthWithUnit
            |%| ", got flex-spec: "
            |%| Eval.fmtGlueFlexSpec
        )
        hSize
        listWidth
        flexSpec
    pure $ Box.Boxed {boxedContents = hBoxElems, boxedDims}
