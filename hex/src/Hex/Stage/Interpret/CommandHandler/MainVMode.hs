module Hex.Stage.Interpret.CommandHandler.MainVMode where

import Hex.Common.HexState.Interface qualified as H.Inter.St
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as H.Sym.Tok
import Hex.Common.Quantity qualified as H.Q
import Hex.Stage.Evaluate.Interface qualified as H.Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as H.Eval.AST
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Interpret.Build.List.Horizontal.Paragraph.Break qualified as H.Inter.B.List.H.Para
import Hex.Stage.Interpret.Build.List.Horizontal.Set qualified as H.Inter.B.List.H
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as H.AllMode
import Hex.Stage.Interpret.CommandHandler.ParaMode qualified as H.Para
import Hex.Stage.Lex.Interface.CharSource (CharSource)
import Hexlude

data VModeCommandResult
  = ContinueMainVMode
  | EndMainVMode

buildMainVList ::
  ( H.Eval.MonadEvaluated m,
    H.Inter.St.MonadHexState m,
    MonadError e m,
    MonadIO m,
    AsType H.AllMode.InterpretError e
  ) =>
  m H.Inter.B.List.VList
buildMainVList = execStateT go (H.Inter.B.List.VList Empty)
  where
    go = do
      streamPreParse <- lift H.Eval.getSource
      command <- lift H.Eval.getCommand
      traceM $ show command
      handleCommandInMainVMode streamPreParse command >>= \case
        EndMainVMode -> pure ()
        ContinueMainVMode -> go

handleCommandInMainVMode ::
  ( Monad m,
    MonadIO m,
    H.Eval.MonadEvaluated m,
    H.Inter.St.MonadHexState m,
    MonadError e m,
    AsType H.AllMode.InterpretError e
  ) =>
  CharSource ->
  H.Eval.AST.Command ->
  StateT H.Inter.B.List.VList m VModeCommandResult
handleCommandInMainVMode oldSrc = \case
  H.Eval.AST.VModeCommand H.Eval.AST.End ->
    pure EndMainVMode
  -- H.Eval.AST.VModeCommand (H.Eval.AST.AddVGlue g) -> do
  --   H.Inter.Eval.evalASTGlue g >>= extendVListStateT . H.Inter.B.List.ListGlue
  --   pure ContinueMainVMode
  -- H.Eval.AST.VModeCommand (H.Eval.AST.AddVRule astRule) -> do
  --   rule <- H.Inter.Eval.evalASTVModeRule astRule
  --   extendVListStateT $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox $ H.Inter.B.Box.RuleContents <$ rule ^. #unRule
  --   pure ContinueMainVMode
  H.Eval.AST.HModeCommand _ -> do
    addPara H.Sym.Tok.Indent
  H.Eval.AST.StartParagraph indentFlag -> do
    addPara indentFlag
  -- -- \par does nothing in vertical mode.
  -- H.Eval.AST.EndParagraph ->
  --   pure ContinueMainVMode
  -- -- <space token> has no effect in vertical modes.
  -- H.Eval.AST.AddSpace ->
  --   pure ContinueMainVMode
  H.Eval.AST.ModeIndependentCommand modeIndependentCommand -> do
    H.AllMode.handleModeIndependentCommand extendVListStateT modeIndependentCommand >>= \case
      H.AllMode.SawEndBox ->
        lift $ throwError $ injectTyped H.AllMode.SawEndBoxInMainVMode
      H.AllMode.DidNotSeeEndBox ->
        pure ContinueMainVMode
  oth ->
    panic $ "Not implemented, outer V mode: " <> show oth
  where
    addPara indentFlag = do
      -- If the command shifts to horizontal mode, run '\indent', and re-read
      -- the stream as if the command hadn't been read.
      lift $ H.Eval.putSource oldSrc
      (endParaReason, paraHList) <- lift $ H.Para.buildParaList indentFlag
      extendVListWithParagraphStateT paraHList
      case endParaReason of
        H.Para.EndParaSawEndParaCommand ->
          pure ContinueMainVMode
        H.Para.EndParaSawLeaveBox ->
          lift $ throwError $ injectTyped H.AllMode.SawEndBoxInMainVModePara

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
    -- hSize <- H.Inter.St.getLengthParameter H.Sym.Tok.HSize
    -- traceM $ "hSize: " <> H.Q.renderLengthWithUnit hSize

    let hSize = H.Q.pt 200
    -- lineTol <- H.Inter.St.getIntParameter H.Sym.Tok.Tolerance
    -- linePen <- H.Inter.St.getIntParameter H.Sym.Tok.LinePenalty
    let lineHLists = H.Inter.B.List.H.Para.breakGreedy hSize hList
    -- H.Inter.B.List.H.Paragraph.breakGreedy hSize lineTol linePen hList

    -- TODO: Get these.
    let boxHeight = H.Q.pt 20
    let boxDepth = H.Q.pt 0

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
    prevDepth <- H.Inter.St.getSpecialLengthParameter H.Sym.Tok.PrevDepth
    blineGlue <- H.Inter.St.getGlueParameter H.Sym.Tok.BaselineSkip
    skipLimit <- H.Inter.St.getLengthParameter H.Sym.Tok.LineSkipLimit
    skip <- H.Inter.St.getGlueParameter H.Sym.Tok.LineSkip
    H.Inter.St.setSpecialLengthParameter H.Sym.Tok.PrevDepth (H.Inter.B.Box.boxDepth b)
    pure $
      H.Inter.B.List.VList $
        if (prevDepth ^. typed @Int) <= -(H.Q.oneKPt ^. typed @Int)
          then accSeq :|> e
          else
            let proposedBaselineLength = (blineGlue ^. #gDimen) ~~ prevDepth ~~ H.Inter.B.Box.boxHeight b
                -- Intuition: set the distance between baselines to \baselineskip, but no
                -- closer than \lineskiplimit [theBaselineLengthMin], in which case
                -- \lineskip [theMinBaselineGlue] is used.
                glue =
                  H.Inter.B.List.ListGlue $
                    if proposedBaselineLength >= skipLimit
                      then blineGlue & #gDimen .~ proposedBaselineLength
                      else skip
             in (accSeq :|> glue) :|> e
  _ ->
    pure (H.Inter.B.List.VList (accSeq :|> e))
