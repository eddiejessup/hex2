module Hex.Stage.Interpret.CommandHandler.MainVMode where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Interpret.Build.List.Horizontal.Paragraph.Break qualified as H.Inter.B.List.H.Para
import Hex.Stage.Interpret.Build.List.Horizontal.Set qualified as H.Inter.B.List.H
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as AllMode
import Hex.Stage.Interpret.CommandHandler.ParaMode qualified as H.Para
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import Hex.Stage.Lex.Interface.LexBuffer (LexBuffer)
import Hex.Stage.Parse.Interface (MonadCommandSource)
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hexlude

data VModeCommandResult
  = ContinueMainVMode
  | EndMainVMode

buildMainVList ::
  forall e m.
  ( Eval.MonadEvaluate m,
    -- We need this because we modify the underlying charsource.
    MonadLexTokenSource m,
    MonadCommandSource m,
    HSt.MonadHexState m,
    MonadError e m,
    AsType AllMode.InterpretError e,
    MonadHexLog m
  ) =>
  m H.Inter.B.List.VList
buildMainVList = execStateT go (H.Inter.B.List.VList Empty)
  where
    go :: StateT H.Inter.B.List.VList m ()
    go = do
      streamPreParse <- lift getSource
      command <- lift $ AllMode.getNextCommandLogged
      handleCommandInMainVMode streamPreParse command >>= \case
        EndMainVMode -> pure ()
        ContinueMainVMode -> go

handleCommandInMainVMode ::
  forall m e.
  ( Monad m,
    MonadHexLog m,
    MonadCommandSource m,
    Eval.MonadEvaluate m,
    HSt.MonadHexState m,
    MonadLexTokenSource m,
    MonadError e m,
    AsType AllMode.InterpretError e
  ) =>
  LexBuffer ->
  Eval.Command ->
  StateT H.Inter.B.List.VList m VModeCommandResult
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
    addPara PT.Indent
  Eval.StartParagraph indentFlag -> do
    addPara indentFlag
  -- \par does nothing in vertical mode.
  Eval.EndParagraph ->
    pure ContinueMainVMode
  -- <space token> has no effect in vertical modes.
  Eval.AddSpace ->
    pure ContinueMainVMode
  Eval.ModeIndependentCommand modeIndependentCommand -> do
    AllMode.handleModeIndependentCommand extendVListStateT modeIndependentCommand >>= \case
      AllMode.SawEndBox ->
        lift $ throwError $ injectTyped AllMode.SawEndBoxInMainVMode
      AllMode.DidNotSeeEndBox ->
        pure ContinueMainVMode
  oth ->
    notImplemented $ "handleCommandInMainVMode, command" <> show oth
  where
    addPara ::
      ( PT.IndentFlag ->
        StateT H.Inter.B.List.VList m VModeCommandResult
      )
    addPara indentFlag = do
      -- If the command shifts to horizontal mode, run '\indent', and re-read
      -- the stream as if the command hadn't been read.
      lift $ putSource oldSrc
      (endParaReason, paraHList) <- lift $ H.Para.buildParaList indentFlag
      extendVListWithParagraphStateT paraHList
      case endParaReason of
        H.Para.EndParaSawEndParaCommand ->
          pure ContinueMainVMode
        H.Para.EndParaSawLeaveBox ->
          lift $ throwError $ injectTyped AllMode.SawEndBoxInMainVModePara

extendVListWithParagraphStateT ::
  ( HSt.MonadHexState m
  ) =>
  H.Inter.B.List.HList ->
  StateT H.Inter.B.List.VList m ()
extendVListWithParagraphStateT paraHList = do
  lineBoxes <- lift $ setAndBreakHListToHBoxes paraHList
  for_ lineBoxes $ \b -> do
    extendVListStateT $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox (H.Inter.B.Box.HBoxContents <$> b)

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

extendVListStateT :: HSt.MonadHexState m => H.Inter.B.List.VListElem -> StateT H.Inter.B.List.VList m ()
extendVListStateT e = get >>= lift . extendVList e >>= put

extendVList ::
  (HSt.MonadHexState m) =>
  H.Inter.B.List.VListElem ->
  H.Inter.B.List.VList ->
  m H.Inter.B.List.VList
extendVList e (H.Inter.B.List.VList accSeq) = case e of
  H.Inter.B.List.VListBaseElem (H.Inter.B.Box.ElemBox b) -> do
    -- Assume we are adding a non-rule box of height h to the vertical list.
    -- Let \prevdepth = p, \lineskiplimit = l, \baselineskip = (b plus y minus z).
    -- Add interline glue, above the new box, of:
    -- If p ≤ −1000 pt:
    --    No glue.
    -- Otherwise, if b−p−h ≥ l:
    --    (b−p−h) plus y minus z
    -- Otherwise:
    --    \lineskip
    -- Then set \prevdepth to the depth of the new box.
    prevDepth <- HSt.getSpecialLengthParameter HSt.Param.PrevDepth
    blineGlue <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.BaselineSkip)
    skipLimit <- HSt.getParameterValue (HSt.Param.LengthQuantParam HSt.Param.LineSkipLimit)
    skip <- HSt.getParameterValue (HSt.Param.GlueQuantParam HSt.Param.LineSkip)
    HSt.setSpecialLengthParameter HSt.Param.PrevDepth (H.Inter.B.Box.boxDepth b)
    pure $
      H.Inter.B.List.VList $
        if (prevDepth ^. typed @Int) <= -(Q.oneKPt ^. typed @Int)
          then accSeq :|> e
          else
            let proposedBaselineLength = (blineGlue ^. #gDimen) ~~ prevDepth ~~ H.Inter.B.Box.boxHeight b
                -- Intuition: set the distance between baselines to \baselineskip, but no
                -- closer than \lineskiplimit [theBaselineLengthMin], in which case
                -- \lineskip [theMinBaselineGlue] is used.
                glue =
                  H.Inter.B.List.ListGlue $
                    if proposedBaselineLength >= skipLimit
                      then blineGlue & #gDimen !~ proposedBaselineLength
                      else skip
             in (accSeq :|> glue) :|> e
  _ ->
    pure (H.Inter.B.List.VList (accSeq :|> e))
