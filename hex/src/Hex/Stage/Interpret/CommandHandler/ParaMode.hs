module Hex.Stage.Interpret.CommandHandler.ParaMode where

import Hex.Common.Codes qualified as H.Codes
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as H.Sym.Tok
import Hex.Common.Quantity qualified as H.Q
import Hex.Stage.Evaluate.Interface qualified as H.Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as H.AST
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as H.AllMode
import Hex.Stage.Lex.Interface.CharSource (CharSource)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

data ParaModeCommandResult
  = ContinuePara
  | EndPara EndParaReason

data EndParaReason
  = EndParaSawEndParaCommand
  | EndParaSawLeaveBox
  deriving stock (Show)

buildParaList ::
  ( H.Eval.MonadEvaluated m,
    HSt.MonadHexState m,
    MonadError e m,
    MonadIO m,
    AsType H.AllMode.InterpretError e
  ) =>
  H.Sym.Tok.IndentFlag ->
  m (EndParaReason, H.Inter.B.List.HList)
buildParaList indentFlag = do
  initList <- case indentFlag of
    H.Sym.Tok.Indent ->
      singleton <$> HSt.getParIndentBox
    H.Sym.Tok.DoNotIndent ->
      pure mempty
  runStateT go (H.Inter.B.List.HList initList)
  where
    go = do
      sPreParse <- lift H.Eval.getSource
      command <- lift H.Eval.getCommand
      traceM $ "In para-mode, saw command: " <> show command
      handleCommandInParaMode sPreParse command >>= \case
        EndPara endReason -> pure endReason
        ContinuePara -> go

handleCommandInParaMode ::
  ( Monad m,
    H.Eval.MonadEvaluated m,
    HSt.MonadHexState m,
    MonadError e m,
    MonadIO m,
    AsType H.AllMode.InterpretError e
  ) =>
  CharSource ->
  H.AST.Command ->
  StateT H.Inter.B.List.HList m ParaModeCommandResult
handleCommandInParaMode oldSrc = \case
  H.AST.VModeCommand _ -> do
    -- Insert the control sequence "\par" into the input. The control
    -- sequence's current meaning will be used, which might no longer be the \par
    -- primitive.
    lift $ H.Eval.putSource oldSrc
    lift $ H.Eval.insertLexTokenToSource Lex.parToken
    pure ContinuePara
  H.AST.HModeCommand (H.AST.AddHGlue g) -> do
    extendHListStateT $ H.Inter.B.List.HVListElem $ H.Inter.B.List.ListGlue g
    pure ContinuePara
  -- H.AST.HModeCommand (H.AST.AddCharacter c) -> do
  --   evalChar <- H.Inter.Eval.evalASTChar c
  --   charBox <- lift $ charAsBox evalChar
  --   extendHListStateT $ H.Inter.B.List.HListHBaseElem $ H.Inter.B.Box.ElemCharacter charBox
  --   pure ContinuePara
  H.AST.HModeCommand (H.AST.AddHRule rule) -> do
    extendHListStateT $ H.Inter.B.List.HVListElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox $ H.Inter.B.Box.RuleContents <$ rule ^. #unRule
    pure ContinuePara
  H.AST.AddSpace -> do
    spaceGlue <- lift $ HSt.currentFontSpaceGlue >>= note (injectTyped H.AllMode.NoFontSelected)
    extendHListStateT $ H.Inter.B.List.HVListElem $ H.Inter.B.List.ListGlue spaceGlue
    pure ContinuePara
  H.AST.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag
    pure ContinuePara
  -- \par: Restricted: does nothing. Unrestricted (this mode): ends mode.
  H.AST.EndParagraph ->
    pure $ EndPara EndParaSawEndParaCommand
  H.AST.ModeIndependentCommand modeIndependentCommand -> do
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
  H.Codes.CharCode ->
  m H.Inter.B.Box.CharBox
charAsBox char = do
  (width, height, depth, _) <- HSt.currentFontCharacter char >>= note (injectTyped H.AllMode.NoFontSelected)
  pure $
    H.Inter.B.Box.CharBox
      H.Inter.B.Box.Box
        { H.Inter.B.Box.contents = char,
          H.Inter.B.Box.boxWidth = width ^. typed @H.Q.Length,
          H.Inter.B.Box.boxHeight = height ^. typed @H.Q.Length,
          H.Inter.B.Box.boxDepth = depth ^. typed @H.Q.Length
        }

hModeStartParagraph ::
  ( HSt.MonadHexState m
  ) =>
  H.Sym.Tok.IndentFlag ->
  StateT H.Inter.B.List.HList m ()
hModeStartParagraph = \case
  H.Sym.Tok.DoNotIndent ->
    pure ()
  -- \indent: An empty box of width \parindent is appended to the current
  -- list, and the space factor is set to 1000.
  -- TODO: Space factor.
  H.Sym.Tok.Indent -> do
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
