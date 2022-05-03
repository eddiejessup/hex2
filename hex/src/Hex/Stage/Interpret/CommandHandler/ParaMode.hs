module Hex.Stage.Interpret.CommandHandler.ParaMode where

import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Interpret.CommandHandler.AllMode (InterpretError (..))
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as H.AllMode
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import Hex.Stage.Lex.Interface.CharSource (CharSource)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Interface (MonadCommandSource)
import Hexlude

data ParaModeCommandResult
  = ContinuePara
  | EndPara EndParaReason

data EndParaReason
  = EndParaSawEndParaCommand
  | EndParaSawLeaveBox
  deriving stock (Show)

buildParaList ::
  forall m e.
  ( Eval.MonadEvaluate m,
    MonadLexTokenSource m,
    MonadCommandSource m,
    HSt.MonadHexState m,
    MonadError e m,
    MonadIO m,
    AsType H.AllMode.InterpretError e
  ) =>
  PT.IndentFlag ->
  m (EndParaReason, H.Inter.B.List.HList)
buildParaList indentFlag = do
  initList <- case indentFlag of
    PT.Indent ->
      singleton <$> HSt.getParIndentBox
    PT.DoNotIndent ->
      pure mempty
  runStateT go (H.Inter.B.List.HList initList)
  where
    go :: StateT H.Inter.B.List.HList m EndParaReason
    go = do
      -- Get the state before reading the command,
      -- in case we need to revert to before the command.
      sPreParse <- lift getSource
      -- Read the next command.
      command <- lift $ Eval.getEvalCommandErrorEOF $ injectTyped UnexpectedEndOfInput
      -- Handle the next command, passing the old state in case we need to revert.
      handleCommandInParaMode sPreParse command >>= \case
        EndPara endReason -> pure endReason
        ContinuePara -> go

handleCommandInParaMode ::
  ( Monad m,
    HSt.MonadHexState m,
    MonadLexTokenSource m,
    MonadError e m,
    MonadIO m,
    AsType H.AllMode.InterpretError e
  ) =>
  CharSource ->
  Eval.Command ->
  StateT H.Inter.B.List.HList m ParaModeCommandResult
handleCommandInParaMode oldSrc = \case
  Eval.VModeCommand _ -> do
    -- Insert the control sequence "\par" into the input. The control
    -- sequence's current meaning will be used, which might no longer be the \par
    -- primitive.
    lift $ putSource oldSrc
    lift $ insertLexTokenToSource Lex.parToken
    pure ContinuePara
  Eval.HModeCommand (Eval.AddHGlue g) -> do
    extendHListStateT $ H.Inter.B.List.HVListElem $ H.Inter.B.List.ListGlue g
    pure ContinuePara
  -- Eval.HModeCommand (Eval.AddCharacter c) -> do
  --   evalChar <- H.Inter.Eval.evalASTChar c
  --   charBox <- lift $ charAsBox evalChar
  --   extendHListStateT $ H.Inter.B.List.HListHBaseElem $ H.Inter.B.Box.ElemCharacter charBox
  --   pure ContinuePara
  Eval.HModeCommand (Eval.AddHRule rule) -> do
    extendHListStateT $ H.Inter.B.List.HVListElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox $ H.Inter.B.Box.RuleContents <$ rule ^. #unRule
    pure ContinuePara
  Eval.AddSpace -> do
    spaceGlue <- lift $ HSt.currentFontSpaceGlue >>= note (injectTyped H.AllMode.NoFontSelected)
    extendHListStateT $ H.Inter.B.List.HVListElem $ H.Inter.B.List.ListGlue spaceGlue
    pure ContinuePara
  Eval.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag
    pure ContinuePara
  -- \par: Restricted: does nothing. Unrestricted (this mode): ends mode.
  Eval.EndParagraph ->
    pure $ EndPara EndParaSawEndParaCommand
  Eval.ModeIndependentCommand modeIndependentCommand -> do
    H.AllMode.handleModeIndependentCommand extendHListVElemStateT modeIndependentCommand <&> \case
      H.AllMode.SawEndBox ->
        EndPara EndParaSawEndParaCommand
      H.AllMode.DidNotSeeEndBox ->
        ContinuePara
  oth ->
    panic $ show oth

charAsBox ::
  ( HSt.MonadHexState m,
    MonadError e m,
    AsType H.AllMode.InterpretError e
  ) =>
  Codes.CharCode ->
  m H.Inter.B.Box.CharBox
charAsBox char = do
  (width, height, depth, _) <- HSt.currentFontCharacter char >>= note (injectTyped H.AllMode.NoFontSelected)
  pure $
    H.Inter.B.Box.CharBox
      H.Inter.B.Box.Box
        { H.Inter.B.Box.contents = char,
          H.Inter.B.Box.boxWidth = width ^. typed @Q.Length,
          H.Inter.B.Box.boxHeight = height ^. typed @Q.Length,
          H.Inter.B.Box.boxDepth = depth ^. typed @Q.Length
        }

hModeStartParagraph ::
  ( HSt.MonadHexState m
  ) =>
  PT.IndentFlag ->
  StateT H.Inter.B.List.HList m ()
hModeStartParagraph = \case
  PT.DoNotIndent ->
    pure ()
  -- \indent: An empty box of width \parindent is appended to the current
  -- list, and the space factor is set to 1000.
  -- TODO: Space factor.
  PT.Indent -> do
    lift HSt.getParIndentBox >>= extendHListStateT

extendHListVElemStateT :: Monad m => H.Inter.B.List.VListElem -> StateT H.Inter.B.List.HList m ()
extendHListVElemStateT e = extendHListStateT (H.Inter.B.List.HVListElem e)

extendHListStateT :: Monad m => H.Inter.B.List.HListElem -> StateT H.Inter.B.List.HList m ()
extendHListStateT e = modify $ extendHList e

extendHList ::
  H.Inter.B.List.HListElem ->
  H.Inter.B.List.HList ->
  H.Inter.B.List.HList
extendHList e (H.Inter.B.List.HList accSeq) = H.Inter.B.List.HList $ accSeq :|> e
