module Hex.Stage.Interpret.VMode where

import Formatting qualified as F
import Hex.Capability.Log.Interface (HexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Mode qualified as HSt.Mode
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.AnyDirection.Evaluate qualified as Eval
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Horizontal.Paragraph.Break.Optimal qualified as Break
import Hex.Stage.Build.Horizontal.Set qualified as List.H
import Hex.Stage.Build.ListBuilder.Interface qualified as Build
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.AllMode qualified as AllMode
import Hex.Stage.Read.Interface qualified as HIn
import Hex.Stage.Read.Interface.CharSourceStack (CharSourceStack)
import Hexlude

data VModeCommandResult
  = ContinueMainVMode
  | EndMainVMode

handleCommandInVMode ::
  forall es.
  ( HexLog :> es,
    HSt.EHexState :> es,
    HIn.HexInput :> es,
    Error AllMode.InterpretError :> es,
    Build.HexListBuilder :> es,
    ListExtractor.ExtractList :> es
  ) =>
  CharSourceStack ->
  HSt.Mode.ModeVariant ->
  Eval.Command ->
  Eff es VModeCommandResult
handleCommandInVMode oldSrc modeVariant command =
  case modeVariant of
    HSt.Mode.InnerModeVariant ->
      notImplemented "Inner V Mode command"
    HSt.Mode.OuterModeVariant -> case command of
      Eval.VModeCommand vModeCommand -> case vModeCommand of
        Eval.End ->
          pure EndMainVMode
        Eval.AddVGlue g -> do
          Build.addVListElement $ ListElem.ListGlue g
          pure ContinueMainVMode
        Eval.Dump ->
          notImplemented "handleCommandInVMode: Dump"
        Eval.EnterHMode ->
          notImplemented "handleCommandInVMode: EnterHMode"
        Eval.AddVLeaders _leadersSpec ->
          notImplemented "handleCommandInVMode: AddVLeaders"
        Eval.AddVRule rule -> do
          Build.addVListElement $ ListElem.VListBaseElem $ BoxElem.ElemBox $ BoxElem.ruleAsBaseBox rule
          pure ContinueMainVMode
        Eval.AddUnwrappedFetchedVBox _fetchedBoxRef ->
          notImplemented "handleCommandInVMode: AddUnwrappedFetchedVBox"
        Eval.AddHAlignedMaterial _boxSpec ->
          notImplemented "VMode: AddHAlignedMaterial"
      Eval.HModeCommand _ ->
        addPara ListExtractor.Indent
      Eval.StartParagraph indentFlag ->
        addPara indentFlag
      -- \par does nothing in vertical mode.
      Eval.EndParagraph ->
        pure ContinueMainVMode
      -- <space token> has no effect in vertical modes.
      Eval.AddSpace ->
        pure ContinueMainVMode
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
            throwError AllMode.SawEndBoxInMainVMode
          AllMode.DidNotSeeEndBox ->
            pure ContinueMainVMode
  where
    addPara :: ListExtractor.IndentFlag -> Eff es VModeCommandResult
    addPara indentFlag = do
      -- Just before switching to horizontal mode to begin scanning a paragraph,
      -- TEX inserts the glue specified by \parskip into the vertical list that
      -- will contain the paragraph, unless that vertical list is empty so far.
      parSkipGlue <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.ParSkip)
      Log.infoLog $ "addPara: Adding \\glue(\\parSkip): " <> F.sformat Q.fmtGlue parSkipGlue

      Build.addVListElement $ ListElem.ListGlue parSkipGlue

      -- If the command shifts to horizontal mode, run '\indent', and re-read
      -- the stream as if the command hadn't been read.
      HIn.putInput oldSrc
      (endParaReason, paraHList) <- ListExtractor.extractParagraphList indentFlag
      extendVListWithParagraphStateT paraHList
      case endParaReason of
        ListExtractor.EndHListSawEndParaCommand ->
          pure ContinueMainVMode
        ListExtractor.EndHListSawLeaveBox ->
          throwError AllMode.SawEndBoxInMainVModePara

extendVListWithParagraphStateT ::
  [HSt.EHexState, Log.HexLog, Build.HexListBuilder] :>> es =>
  ListElem.HList ->
  Eff es ()
extendVListWithParagraphStateT paraHList = do
  lineBoxes <- setAndBreakHListToHBoxes paraHList
  for_ lineBoxes $ \b ->
    Build.addVListElement $ ListElem.VListBaseElem $ BoxElem.ElemBox $ BoxElem.BaseBox (BoxElem.HBoxContents <$> b)

setAndBreakHListToHBoxes ::
  ( [HSt.EHexState, Log.HexLog] :>> es
  ) =>
  ListElem.HList ->
  Eff es (Seq (Box.Box BoxElem.HBoxElemSeq))
setAndBreakHListToHBoxes hList = do
  hSize <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.HSize)
  lineTol <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Tolerance)
  linePen <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.LinePenalty)
  lineHLists <- Break.breakHListOptimally hSize lineTol linePen hList
  for lineHLists $ \lineHList -> do
    let listWidth = ListElem.hListNaturalWidth lineHList
        (hBoxElems, flexSpec) = List.H.setList lineHList hSize

        -- TODO: Implement proper interline glue.
        boxHeight = BoxElem.hBoxNaturalHeight hBoxElems
        boxDepth = BoxElem.hBoxNaturalDepth hBoxElems
        -- TODO: Is this correct?
        boxWidth = hSize
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
    pure $ Box.Box {contents = hBoxElems, boxWidth, boxHeight, boxDepth}
