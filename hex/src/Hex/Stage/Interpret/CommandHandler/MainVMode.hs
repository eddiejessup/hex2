module Hex.Stage.Interpret.CommandHandler.MainVMode where

import Hex.Common.HexState.Interface qualified as H.Inter.St
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
import Hex.Stage.Lex.Interface.CharSource (CharSource)
import Hex.Stage.Parse.Interface (MonadCommandSource)
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hexlude
import Hex.Capability.Log.Interface (MonadHexLog)

data VModeCommandResult
  = ContinueMainVMode
  | EndMainVMode

buildMainVList ::
  forall e m.
  ( Eval.MonadEvaluate m,
    -- We need this because we modify the underlying charsource.
    MonadLexTokenSource m,
    MonadCommandSource m,
    H.Inter.St.MonadHexState m,
    MonadError e m,
    AsType AllMode.InterpretError e,
    MonadIO m,
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
    MonadIO m,
    MonadHexLog m,
    MonadCommandSource m,
    Eval.MonadEvaluate m,
    H.Inter.St.MonadHexState m,
    MonadLexTokenSource m,
    MonadError e m,
    AsType AllMode.InterpretError e
  ) =>
  CharSource ->
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
  ( H.Inter.St.MonadHexState m
  ) =>
  H.Inter.B.List.HList ->
  StateT H.Inter.B.List.VList m ()
extendVListWithParagraphStateT paraHList = do
  lineBoxes <- lift $ setAndBreakHListToHBoxes paraHList
  for_ lineBoxes $ \b -> do
    extendVListStateT $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox (H.Inter.B.Box.HBoxContents <$> b)

setAndBreakHListToHBoxes ::
  ( H.Inter.St.MonadHexState m
  ) =>
  H.Inter.B.List.HList ->
  m (Seq (H.Inter.B.Box.Box H.Inter.B.Box.HBoxElemSeq))
setAndBreakHListToHBoxes hList =
  do
    -- hSize <- H.Inter.St.getScopedParameterValue PT.HSize

    let hSize = Q.pt 200
    -- lineTol <- H.Inter.St.getIntParameter PT.Tolerance
    -- linePen <- H.Inter.St.getIntParameter PT.LinePenalty
    let lineHLists = H.Inter.B.List.H.Para.breakGreedy hSize hList
    -- H.Inter.B.List.H.Paragraph.breakGreedy hSize lineTol linePen hList

    -- TODO: Get these.
    let boxHeight = Q.pt 20
    let boxDepth = Q.pt 0

    pure $
      lineHLists <&> \lineHList ->
        let (hBoxElems, _) = H.Inter.B.List.H.setList lineHList hSize
         in H.Inter.B.Box.Box {contents = hBoxElems, boxWidth = hSize, boxHeight, boxDepth}

extendVListStateT :: H.Inter.St.MonadHexState m => H.Inter.B.List.VListElem -> StateT H.Inter.B.List.VList m ()
extendVListStateT e = get >>= lift . extendVList e >>= put

extendVList ::
  (H.Inter.St.MonadHexState m) =>
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
    prevDepth <- H.Inter.St.getSpecialLengthParameter PT.PrevDepth
    blineGlue <- H.Inter.St.getScopedParameterValue PT.BaselineSkip
    skipLimit <- H.Inter.St.getScopedParameterValue PT.LineSkipLimit
    skip <- H.Inter.St.getScopedParameterValue PT.LineSkip
    H.Inter.St.setSpecialLengthParameter PT.PrevDepth (H.Inter.B.Box.boxDepth b)
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
