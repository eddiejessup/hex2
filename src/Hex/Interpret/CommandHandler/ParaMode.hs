module Hex.Interpret.CommandHandler.ParaMode where

import Data.Sequence (Seq (..))
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Interpret.Evaluate.Impl qualified as H.Inter.Eval
import Hex.Interpret.CommandHandler.AllMode qualified as H.AllMode
import Hex.Parse.AST qualified as H.AST
import Hex.Parse.MonadParse.Interface qualified as H.Par.Par
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Protolude
import Data.Sequence qualified as Seq
import Hex.Lex.Types qualified as H.Lex
import Hex.Codes qualified as H.Codes
import Hex.Interpret.Evaluate.Evaluated qualified as H.Inter.Eval
import Hex.Parse.CharSource qualified as H.Par.ChrSrc
import Hex.MonadHexState.Interface qualified as H.St
import Hex.MonadHexState.Helpers qualified as H.St

data ParaModeCommandResult
  = ContinuePara
  | EndPara EndParaReason

data EndParaReason
  = EndParaSawEndParaCommand
  | EndParaSawLeaveBox
  deriving stock (Show)

buildParaList
    :: ( H.Par.Par.MonadParse m, H.St.MonadHexState m
       )
    => H.Sym.Tok.IndentFlag
    -> m (EndParaReason, H.Inter.B.List.HList)
buildParaList indentFlag = do
    initList <- case indentFlag of
        H.Sym.Tok.Indent ->
          Seq.singleton <$> H.St.getParIndentBox
        H.Sym.Tok.DoNotIndent ->
          pure mempty
    runStateT go (H.Inter.B.List.HList initList)
  where
    go = do
      sPreParse <- lift H.Par.Par.getStream
      command <- lift H.Par.Par.parseCommand
      handleCommandInParaMode sPreParse command >>= \case
        EndPara endReason -> pure endReason
        ContinuePara -> go

handleCommandInParaMode ::
  ( Monad m, H.Par.Par.MonadParse m
  ) =>
  H.Par.ChrSrc.CharSource ->
  H.AST.Command ->
  StateT H.Inter.B.List.HList m ParaModeCommandResult
handleCommandInParaMode oldSrc = \case
  H.AST.VModeCommand _ -> do
    -- Insert the control sequence "\par" into the input. The control
    -- sequence's current meaning will be used, which might no longer be the \par
    -- primitive.
    lift $ H.Par.Par.putStream oldSrc
    lift $ H.Par.Par.insertLexTokenToStream H.Lex.parToken
    pure ContinuePara
  H.AST.HModeCommand (H.AST.AddHGlue g) -> do
    H.Inter.Eval.evalASTGlue g >>= extendHListStateT . H.Inter.B.List.HVListElem . H.Inter.B.List.ListGlue
    pure ContinuePara
  H.AST.HModeCommand (H.AST.AddCharacter c) -> do
    evalChar <- H.Inter.Eval.evalASTChar c
    charBox <- charAsBox evalChar
    extendHListStateT $ H.Inter.B.List.HListHBaseElem $ H.Inter.B.Box.ElemCharacter charBox
    pure ContinuePara
  H.AST.HModeCommand (H.AST.AddHRule rule) -> do
    H.Inter.Eval.evalASTHModeRule rule >>= extendHListStateT . H.Inter.B.List.HVListElem . H.Inter.B.List.VListBaseElem . H.Inter.B.Box.ElemRule
    pure ContinuePara
  H.AST.AddSpace -> do
    spaceGlue >>= extendHListStateT . H.Inter.B.List.HVListElem . H.Inter.B.List.ListGlue
    pure ContinuePara
  H.AST.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag
    pure ContinuePara
  -- \par: Restricted: does nothing. Unrestricted (this mode): ends mode.
  H.AST.EndParagraph ->
    pure $ EndPara EndParaSawEndParaCommand
  H.AST.ModeIndependentCommand modeIndependentCommand -> do
    lift (H.AllMode.handleModeIndependentCommand modeIndependentCommand) <&> \case
      H.AllMode.SawEndBox ->
        EndPara EndParaSawEndParaCommand
      H.AllMode.DidNotSeeEndBox ->
        ContinuePara
  oth ->
    panic $ show oth

charAsBox
    :: ( Monad m
       )
    => H.Codes.CharCode
    -> m H.Inter.B.Box.Character
charAsBox char = do
    undefined
    -- fontMetrics <- currentFontMetrics
    -- let toSP = TFM.designScaleSP fontMetrics
    -- TFM.Character { TFM.width, TFM.height, TFM.depth } <-
    --     note (injectTyped $ ConfigError "No such character")
    --     (IntMap.lookup (fromIntegral $ Code.codeWord char) (characters fontMetrics))
    -- pure B.Character { B.char       = char
    --                  , B.charWidth  = toSP width
    --                  , B.charHeight = toSP height
    --                  , B.charDepth  = toSP depth
    --                  }

spaceGlue
    :: ( Monad m
       )
    => m (H.Inter.Eval.Glue H.Inter.Eval.Length)
spaceGlue = do
  undefined
  -- fontMetrics@TexFont { spacing, spaceStretch, spaceShrink } <- currentFontMetrics
  -- let toSP   = TFM.designScaleSP fontMetrics
  --     toFlex = toSP >>> fromIntegral >>> BL.finiteFlex
  -- pure BL.Glue { BL.dimen   = toSP spacing
  --              , BL.stretch = toFlex spaceStretch
  --              , BL.shrink  = toFlex spaceShrink
  --              }

hModeStartParagraph
    :: ( Monad m
       )
    => H.Sym.Tok.IndentFlag
    -> StateT H.Inter.B.List.HList m ()
hModeStartParagraph = \case
    H.Sym.Tok.DoNotIndent ->
      pure ()
    -- \indent: An empty box of width \parindent is appended to the current
    -- list, and the space factor is set to 1000.
    -- TODO: Space factor.
    H.Sym.Tok.Indent -> do
      -- parIndentBoxElem <- uses (typed @Config) parIndentBox
      parIndentBoxElem <- undefined
      extendHListStateT parIndentBoxElem

extendHListStateT :: Monad m => H.Inter.B.List.HListElem -> StateT H.Inter.B.List.HList m ()
extendHListStateT e = modify $ extendHList e

extendHList ::
  H.Inter.B.List.HListElem ->
  H.Inter.B.List.HList ->
  H.Inter.B.List.HList
extendHList e (H.Inter.B.List.HList accSeq) = H.Inter.B.List.HList $ accSeq :|> e
