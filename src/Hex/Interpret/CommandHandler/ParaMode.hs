module Hex.Interpret.CommandHandler.ParaMode where

import Hex.Codes qualified as H.Codes
import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Evaluate.MonadEvaluated.Interface qualified as H.Eval
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Interpret.CommandHandler.AllMode qualified as H.AllMode
import Hex.Lex.Types qualified as H.Lex
import Hex.MonadHexState.Helpers qualified as H.St
import Hex.MonadHexState.Interface qualified as H.St
import Hex.Parse.CharSource qualified as H.Par.ChrSrc
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
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
    H.St.MonadHexState m,
    MonadError e m,
    MonadIO m,
    AsType H.AllMode.InterpretError e
  ) =>
  H.Sym.Tok.IndentFlag ->
  m (EndParaReason, H.Inter.B.List.HList)
buildParaList indentFlag = do
  initList <- case indentFlag of
    H.Sym.Tok.Indent ->
      singleton <$> H.St.getParIndentBox
    H.Sym.Tok.DoNotIndent ->
      pure mempty
  runStateT go (H.Inter.B.List.HList initList)
  where
    go = do
      sPreParse <- lift H.Eval.getStream
      command <- lift H.Eval.parseCommand
      handleCommandInParaMode sPreParse command >>= \case
        EndPara endReason -> pure endReason
        ContinuePara -> go

handleCommandInParaMode ::
  ( Monad m,
    H.Eval.MonadEvaluated m,
    H.St.MonadHexState m,
    MonadError e m,
    MonadIO m,
    AsType H.AllMode.InterpretError e
  ) =>
  H.Par.ChrSrc.CharSource ->
  H.Syn.Command 'H.Syn.Evaluated ->
  StateT H.Inter.B.List.HList m ParaModeCommandResult
handleCommandInParaMode oldSrc = \case
  H.Syn.VModeCommand _ -> do
    -- Insert the control sequence "\par" into the input. The control
    -- sequence's current meaning will be used, which might no longer be the \par
    -- primitive.
    lift $ H.Eval.putStream oldSrc
    lift $ H.Eval.insertLexTokenToStream H.Lex.parToken
    pure ContinuePara
  H.Syn.HModeCommand (H.Syn.AddHGlue g) -> do
    extendHListStateT $ H.Inter.B.List.HVListElem $ H.Inter.B.List.ListGlue g
    pure ContinuePara
  -- H.Syn.HModeCommand (H.Syn.AddCharacter c) -> do
  --   evalChar <- H.Inter.Eval.evalASTChar c
  --   charBox <- lift $ charAsBox evalChar
  --   extendHListStateT $ H.Inter.B.List.HListHBaseElem $ H.Inter.B.Box.ElemCharacter charBox
  --   pure ContinuePara
  H.Syn.HModeCommand (H.Syn.AddHRule rule) -> do
    extendHListStateT $ H.Inter.B.List.HVListElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox $ H.Inter.B.Box.RuleContents <$ rule ^. #unRule
    pure ContinuePara
  H.Syn.AddSpace -> do
    spaceGlue <- lift $ H.St.currentFontSpaceGlue >>= note (injectTyped H.AllMode.NoFontSelected)
    extendHListStateT $ H.Inter.B.List.HVListElem $ H.Inter.B.List.ListGlue spaceGlue
    pure ContinuePara
  H.Syn.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag
    pure ContinuePara
  -- \par: Restricted: does nothing. Unrestricted (this mode): ends mode.
  H.Syn.EndParagraph ->
    pure $ EndPara EndParaSawEndParaCommand
  H.Syn.ModeIndependentCommand modeIndependentCommand -> do
    H.AllMode.handleModeIndependentCommand extendHListVElemStateT modeIndependentCommand <&> \case
      H.AllMode.SawEndBox ->
        EndPara EndParaSawEndParaCommand
      H.AllMode.DidNotSeeEndBox ->
        ContinuePara
  _oth ->
    panic "Not implemented: v-mode command"

charAsBox ::
  ( H.St.MonadHexState m,
    MonadError e m,
    AsType H.AllMode.InterpretError e
  ) =>
  H.Codes.CharCode ->
  m H.Inter.B.Box.CharBox
charAsBox char = do
  (width, height, depth, _) <- H.St.currentFontCharacter char >>= note (injectTyped H.AllMode.NoFontSelected)
  pure $
    H.Inter.B.Box.CharBox
      H.Inter.B.Box.Box
        { H.Inter.B.Box.contents = char,
          H.Inter.B.Box.boxWidth = width ^. typed @H.Q.Length,
          H.Inter.B.Box.boxHeight = height ^. typed @H.Q.Length,
          H.Inter.B.Box.boxDepth = depth ^. typed @H.Q.Length
        }

hModeStartParagraph ::
  ( H.St.MonadHexState m
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
    lift H.St.getParIndentBox >>= extendHListStateT

extendHListVElemStateT :: Monad m => H.Inter.B.List.VListElem -> StateT H.Inter.B.List.HList m ()
extendHListVElemStateT e = extendHListStateT (H.Inter.B.List.HVListElem e)

extendHListStateT :: Monad m => H.Inter.B.List.HListElem -> StateT H.Inter.B.List.HList m ()
extendHListStateT e = modify $ extendHList e

extendHList ::
  H.Inter.B.List.HListElem ->
  H.Inter.B.List.HList ->
  H.Inter.B.List.HList
extendHList e (H.Inter.B.List.HList accSeq) = H.Inter.B.List.HList $ accSeq :|> e
