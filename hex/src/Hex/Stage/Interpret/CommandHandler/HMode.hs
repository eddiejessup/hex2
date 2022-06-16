module Hex.Stage.Interpret.CommandHandler.HMode where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
import Hex.Stage.Build.ListBuilder.Interface qualified as Build
import Hex.Stage.Build.ListElem qualified as H.Inter.B.List
import Hex.Stage.Build.ListExtractor.Interface (MonadHexListExtractor)
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.CommandHandler.AllMode qualified as AllMode
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Lex.Interface.LexBuffer (LexBuffer)
import Hexlude

data HModeCommandResult
  = ContinueHMode
  | EndHList ListExtractor.EndHListReason
  deriving stock (Show, Generic)

handleCommandInHMode ::
  ( Monad m,
    HSt.MonadHexState m,
    MonadLexTokenSource m,
    MonadError e m,
    AsType AllMode.InterpretError e,
    Log.MonadHexLog m,
    Build.MonadHListBuilder m,
    MonadHexListExtractor m
  ) =>
  LexBuffer ->
  ListExtractor.ModeContext ->
  Eval.Command ->
  m HModeCommandResult
handleCommandInHMode oldSrc modeCtx = \case
  Eval.VModeCommand _ -> case modeCtx of
    ListExtractor.OuterModeContext -> do
      -- Insert the control sequence "\par" into the input. The control
      -- sequence's current meaning will be used, which might no longer be the \par
      -- primitive.
      putSource oldSrc
      insertLexTokenToSource Lex.parToken
      pure ContinueHMode
    ListExtractor.InnerModeContext -> do
      throwError $ injectTyped $ AllMode.VModeCommandInInnerHMode
  Eval.HModeCommand hModeCommand -> case hModeCommand of
    Eval.AddHGlue g -> do
      Build.addHListElement $ H.Inter.B.List.HVListElem $ H.Inter.B.List.ListGlue g
      pure ContinueHMode
    -- Eval.HModeCommand (Eval.AddCharacter c) -> do
    --   evalChar <- H.Inter.Eval.evalASTChar c
    --   charBox <- lift $ charAsBox evalChar
    --   Build.addHListElement $ H.Inter.B.List.HListHBaseElem $ H.Inter.B.Box.ElemCharacter charBox
    --   pure ContinueHMode
    Eval.AddHRule rule -> do
      Build.addHListElement $ H.Inter.B.List.HVListElem $ H.Inter.B.List.VListBaseElem $ H.Inter.B.Box.ElemBox $ H.Inter.B.Box.RuleContents <$ rule ^. #unRule
      pure ContinueHMode
    Eval.AddControlSpace ->
      notImplemented "AddControlSpace"
    Eval.AddCharacter _charCodeRef ->
      notImplemented "AddCharacter"
    Eval.AddAccentedCharacter _n _assignments _mayCharCodeRef ->
      notImplemented "AddAccentedCharacter"
    Eval.AddItalicCorrection ->
      notImplemented "AddItalicCorrection"
    Eval.AddDiscretionaryText _discretionaryText ->
      notImplemented "AddDiscretionaryText"
    Eval.AddDiscretionaryHyphen ->
      notImplemented "AddDiscretionaryHyphen"
    Eval.EnterMathMode ->
      notImplemented "EnterMathMode"
    Eval.AddHLeaders _leadersSpec ->
      notImplemented "AddHLeaders"
    Eval.AddUnwrappedFetchedHBox _fetchedBoxRef ->
      notImplemented "AddUnwrappedFetchedHBox"
  Eval.AddSpace -> do
    spaceGlue <- HSt.currentFontSpaceGlue >>= note (injectTyped AllMode.NoFontSelected)
    Build.addHListElement $ H.Inter.B.List.HVListElem $ H.Inter.B.List.ListGlue spaceGlue
    pure ContinueHMode
  Eval.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag
    pure ContinueHMode
  -- \par: Restricted: does nothing. Unrestricted: ends mode.
  Eval.EndParagraph -> pure $ case modeCtx of
    ListExtractor.OuterModeContext ->
      EndHList ListExtractor.EndHListSawEndParaCommand
    ListExtractor.InnerModeContext ->
      ContinueHMode
  Eval.ModeIndependentCommand modeIndependentCommand ->
    AllMode.handleModeIndependentCommand modeIndependentCommand <&> \case
      AllMode.SawEndBox ->
        EndHList ListExtractor.EndHListSawEndParaCommand
      AllMode.DidNotSeeEndBox ->
        ContinueHMode
  Eval.ShowToken _lexToken -> notImplemented "HMode: ShowToken"
  Eval.ShowBox _n -> notImplemented "HMode: ShowBox"
  Eval.ShowLists -> notImplemented "HMode: ShowLists"
  Eval.ShowTheInternalQuantity _internalQuantity -> notImplemented "HMode: ShowTheInternalQuantity"
  Eval.ShipOut _box -> notImplemented "HMode: ShipOut"
  Eval.AddMark _text -> notImplemented "HMode: AddMark"
  -- Eval.AddInsertion _n _vModeMaterial -> notImplemented "HMode: AddInsertion"
  -- Eval.AddAdjustment _vModeMaterial -> notImplemented "HMode: AddAdjustment"
  -- Eval.AddAlignedMaterial _desiredLength _alignMaterial _hModeCommand1 _hModeCommand2 -> notImplemented "HMode: AddAlignedMaterial"

charAsBox ::
  ( HSt.MonadHexState m,
    MonadError e m,
    AsType AllMode.InterpretError e
  ) =>
  Codes.CharCode ->
  m H.Inter.B.Box.CharBox
charAsBox char = do
  (width, height, depth, _) <- HSt.currentFontCharacter char >>= note (injectTyped AllMode.NoFontSelected)
  pure $
    H.Inter.B.Box.CharBox
      H.Inter.B.Box.Box
        { H.Inter.B.Box.contents = char,
          H.Inter.B.Box.boxWidth = width ^. typed @Q.Length,
          H.Inter.B.Box.boxHeight = height ^. typed @Q.Length,
          H.Inter.B.Box.boxDepth = depth ^. typed @Q.Length
        }

hModeStartParagraph ::
  ( HSt.MonadHexState m,
    Build.MonadHListBuilder m
  ) =>
  ListExtractor.IndentFlag ->
  m ()
hModeStartParagraph = \case
  ListExtractor.DoNotIndent ->
    pure ()
  -- \indent: An empty box of width \parindent is appended to the current
  -- list, and the space factor is set to 1000.
  -- TODO: Space factor.
  ListExtractor.Indent -> do
    HSt.getParIndentBox >>= Build.addHListElement
