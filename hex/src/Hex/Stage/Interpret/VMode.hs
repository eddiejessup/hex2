module Hex.Stage.Interpret.VMode where

import Hex.Capability.Log.Interface (HexLog)
import Hex.Common.Box qualified as Box
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexInput.Interface.CharSourceStack (CharSourceStack)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Stage.Build.BoxElem qualified as Box
import Hex.Stage.Build.Horizontal.Paragraph.Break qualified as List.H.Para
import Hex.Stage.Build.Horizontal.Set qualified as List.H
import Hex.Stage.Build.ListBuilder.Interface qualified as Build
import Hex.Stage.Build.ListElem qualified as List
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.AllMode qualified as AllMode
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hexlude

data VModeCommandResult
  = ContinueMainVMode
  | EndMainVMode

handleCommandInMainVMode ::
  forall es.
  ( HexLog :> es,
    HSt.EHexState :> es,
    HIn.HexInput :> es,
    Error AllMode.InterpretError :> es,
    Build.HexListBuilder :> es,
    ListExtractor.ExtractHList :> es
  ) =>
  CharSourceStack ->
  Eval.Command ->
  Eff es VModeCommandResult
handleCommandInMainVMode oldSrc = \case
  Eval.VModeCommand vModeCommand -> case vModeCommand of
    Uneval.End ->
      pure EndMainVMode
    Uneval.AddVGlue _g -> do
      notImplemented "handleCommandInMainVMode: AddVGlue"
    -- H.Inter.Eval.evalASTGlue g >>= extendVListStateT . List.ListGlue
    --   pure ContinueMainVMode
    --  (Eval.AddVRule astRule) -> do
    --   rule <- H.Inter.Eval.evalASTVModeRule astRule
    --   extendVListStateT $ List.VListBaseElem $ Box.ElemBox $ Box.RuleContents <$ rule ^. #unRule
    --   pure ContinueMainVMode
    Uneval.Dump ->
      notImplemented "handleCommandInMainVMode: Dump"
    Uneval.EnterHMode ->
      notImplemented "handleCommandInMainVMode: EnterHMode"
    Uneval.AddVLeaders _leadersSpec ->
      notImplemented "handleCommandInMainVMode: AddVLeaders"
    Uneval.AddVRule _rule ->
      notImplemented "handleCommandInMainVMode: AddVRule"
    Uneval.AddUnwrappedFetchedVBox _fetchedBoxRef ->
      notImplemented "handleCommandInMainVMode: AddUnwrappedFetchedVBox"
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
  Eval.ModeIndependentCommand modeIndependentCommand ->
    AllMode.handleModeIndependentCommand modeIndependentCommand >>= \case
      AllMode.SawEndBox ->
        throwError AllMode.SawEndBoxInMainVMode
      AllMode.DidNotSeeEndBox ->
        pure ContinueMainVMode
  Eval.ShowToken _lexToken -> notImplemented "HMode: ShowToken"
  Eval.ShowBox _n -> notImplemented "HMode: ShowBox"
  Eval.ShowLists -> notImplemented "HMode: ShowLists"
  Eval.ShowTheInternalQuantity _internalQuantity -> notImplemented "HMode: ShowTheInternalQuantity"
  Eval.ShipOut _box -> notImplemented "HMode: ShipOut"
  Eval.AddMark _text -> notImplemented "HMode: AddMark"
  -- Eval.AddInsertion _n _vModeMaterial -> notImplemented "HMode: AddInsertion"
  -- Eval.AddAdjustment _vModeMaterial -> notImplemented "HMode: AddAdjustment"
  -- Eval.AddAlignedMaterial _desiredLength _alignMaterial _hModeCommand1 _hModeCommand2 -> notImplemented "HMode: AddAlignedMaterial"
  where
    addPara :: ListExtractor.IndentFlag -> Eff es VModeCommandResult
    addPara indentFlag = do
      -- Just before switching to horizontal mode to begin scanning a paragraph,
      -- TEX inserts the glue specified by \parskip into the vertical list that
      -- will contain the paragraph, unless that vertical list is empty so far.
      parSkipGlue <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.ParSkip)
      Build.addVListElement $ List.ListGlue parSkipGlue

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
  ( HSt.EHexState :> es,
    Build.HexListBuilder :> es
  ) =>
  List.HList ->
  Eff es ()
extendVListWithParagraphStateT paraHList = do
  lineBoxes <- setAndBreakHListToHBoxes paraHList
  for_ lineBoxes $ \b ->
    Build.addVListElement $ List.VListBaseElem $ Box.ElemBox $ Box.BaseBox (Box.HBoxContents <$> b)

setAndBreakHListToHBoxes ::
  ( HSt.EHexState :> es
  ) =>
  List.HList ->
  Eff es (Seq (Box.Box Box.HBoxElemSeq))
setAndBreakHListToHBoxes hList = do
  hSize <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.HSize)
  lineTol <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Tolerance)
  linePen <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.LinePenalty)
  let lineHLists = List.H.Para.breakGreedy hSize lineTol linePen hList

  pure $
    lineHLists <&> \lineHList ->
      let (hBoxElems, _) = List.H.setList lineHList hSize
          -- TODO: Implement proper interline glue.
          boxHeight = Box.hBoxNaturalHeight hBoxElems
          boxDepth = Box.hBoxNaturalDepth hBoxElems
          -- TODO: Is this correct?
          boxWidth = hSize
       in Box.Box {contents = hBoxElems, boxWidth, boxHeight, boxDepth}
