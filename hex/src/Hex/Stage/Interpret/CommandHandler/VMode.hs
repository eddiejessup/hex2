module Hex.Stage.Interpret.CommandHandler.VMode where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hex.Stage.Build.Horizontal.Paragraph.Break qualified as H.Inter.B.List.H.Para
import Hex.Stage.Build.Horizontal.Set qualified as H.Inter.B.List.H
import Hex.Stage.Build.ListBuilder.Interface qualified as Build
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as AllMode
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import Hex.Stage.Lex.Interface.LexBuffer (LexBuffer)
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hexlude

data VModeCommandResult
  = ContinueMainVMode
  | EndMainVMode

handleCommandInMainVMode ::
  forall m e.
  ( Monad m,
    MonadHexLog m,
    HSt.MonadHexState m,
    MonadLexTokenSource m,
    MonadError e m,
    AsType AllMode.InterpretError e,
    Build.MonadHexListBuilder m,
    ListExtractor.MonadHexListExtractor m
  ) =>
  LexBuffer ->
  Eval.Command ->
  m VModeCommandResult
handleCommandInMainVMode oldSrc = \case
  Eval.VModeCommand Uneval.End ->
    pure EndMainVMode
  -- Eval.VModeCommand (Eval.AddVGlue g) -> do
  --   H.Inter.Eval.evalASTGlue g >>= extendVListStateT . H.Inter.B.List.ListGlue
  --   pure ContinueMainVMode
  -- Eval.VModeCommand (Eval.AddVRule astRule) -> do
  --   rule <- H.Inter.Eval.evalASTVModeRule astRule
  --   extendVListStateT $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox $ H.Inter.B.Box.RuleContents <$ rule ^. #unRule
  --   pure ContinueMainVMode
  Eval.HModeCommand _ -> do
    addPara ListExtractor.Indent
  Eval.StartParagraph indentFlag -> do
    addPara indentFlag
  -- \par does nothing in vertical mode.
  Eval.EndParagraph ->
    pure ContinueMainVMode
  -- <space token> has no effect in vertical modes.
  Eval.AddSpace ->
    pure ContinueMainVMode
  Eval.ModeIndependentCommand modeIndependentCommand -> do
    AllMode.handleModeIndependentCommand modeIndependentCommand >>= \case
      AllMode.SawEndBox ->
        throwError $ injectTyped AllMode.SawEndBoxInMainVMode
      AllMode.DidNotSeeEndBox ->
        pure ContinueMainVMode
  oth ->
    notImplemented $ "handleCommandInMainVMode, command" <> show oth
  where
    addPara :: ListExtractor.IndentFlag -> m VModeCommandResult
    addPara indentFlag = do
      -- If the command shifts to horizontal mode, run '\indent', and re-read
      -- the stream as if the command hadn't been read.
      putSource oldSrc
      (endParaReason, paraHList) <- ListExtractor.extractParagraphList indentFlag
      extendVListWithParagraphStateT paraHList
      case endParaReason of
        ListExtractor.EndHListSawEndParaCommand ->
          pure ContinueMainVMode
        ListExtractor.EndHListSawLeaveBox ->
          throwError $ injectTyped AllMode.SawEndBoxInMainVModePara

extendVListWithParagraphStateT ::
  ( HSt.MonadHexState m,
    Build.MonadHexListBuilder m
  ) =>
  H.Inter.B.List.HList ->
  m ()
extendVListWithParagraphStateT paraHList = do
  lineBoxes <- setAndBreakHListToHBoxes paraHList
  for_ lineBoxes $ \b -> do
    Build.addVListElement $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox (H.Inter.B.Box.HBoxContents <$> b)

setAndBreakHListToHBoxes ::
  ( HSt.MonadHexState m
  ) =>
  H.Inter.B.List.HList ->
  m (Seq (H.Inter.B.Box.Box H.Inter.B.Box.HBoxElemSeq))
setAndBreakHListToHBoxes hList =
  do
    hSize <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.HSize)
    lineTol <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Tolerance)
    linePen <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.LinePenalty)
    let lineHLists = H.Inter.B.List.H.Para.breakGreedy hSize lineTol linePen hList

    -- TODO: Get these.
    let boxHeight = Q.pt 20
    let boxDepth = Q.pt 0

    pure $
      lineHLists <&> \lineHList ->
        let (hBoxElems, _) = H.Inter.B.List.H.setList lineHList hSize
         in H.Inter.B.Box.Box {contents = hBoxElems, boxWidth = hSize, boxHeight, boxDepth}
