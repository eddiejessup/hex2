module Hex.Stage.Interpret.HMode where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.ListBuilder.Interface qualified as Build
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Interpret.AllMode qualified as AllMode
import Hex.Stage.Read.Interface qualified as HIn
import Hex.Stage.Read.Interface.CharSourceStack (CharSourceStack)
import Hexlude

data HModeCommandResult
  = ContinueHMode
  | EndHList ListExtractor.EndHListReason
  deriving stock (Show, Generic)

handleCommandInHMode ::
  ( HSt.EHexState :> es,
    HIn.HexInput :> es,
    Error AllMode.InterpretError :> es,
    Log.HexLog :> es,
    Build.HListBuilder :> es,
    Build.HexListBuilder :> es,
    ListExtractor.ExtractHList :> es
  ) =>
  CharSourceStack ->
  ListExtractor.ModeContext ->
  Eval.Command ->
  Eff es HModeCommandResult
handleCommandInHMode oldSrc modeCtx = \case
  Eval.VModeCommand _ -> case modeCtx of
    ListExtractor.OuterModeContext -> do
      -- Insert the control sequence "\par" into the input. The control
      -- sequence's current meaning will be used, which might no longer be the \par
      -- primitive.
      HIn.putInput oldSrc
      HIn.insertLexToken LT.parToken
      pure ContinueHMode
    ListExtractor.InnerModeContext -> do
      throwError $ AllMode.VModeCommandInInnerHMode
  Eval.HModeCommand hModeCommand -> case hModeCommand of
    Eval.AddHGlue g -> do
      Build.addHListElement $ ListElem.HVListElem $ ListElem.ListGlue g
      pure ContinueHMode
    Eval.AddCharacter c -> do
      charBox <- charAsBox c
      Build.addHListElement $ ListElem.HListHBaseElem $ BoxElem.ElemCharacter charBox
      pure ContinueHMode
    Eval.AddHRule rule -> do
      Build.addHListElement $ ListElem.HVListElem $ ListElem.VListBaseElem $ BoxElem.ElemBox $ BoxElem.ruleAsBaseBox rule
      pure ContinueHMode
    Eval.AddControlSpace ->
      notImplemented "AddControlSpace"
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
    spaceGlue <- HSt.currentFontSpaceGlue >>= note (AllMode.NoFontSelected)
    Build.addHListElement $ ListElem.HVListElem $ ListElem.ListGlue spaceGlue
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
  ( HSt.EHexState :> es,
    Error AllMode.InterpretError :> es
  ) =>
  Codes.CharCode ->
  Eff es Box.CharBox
charAsBox char = do
  (width, height, depth, _) <- HSt.currentFontCharacter char >>= note (AllMode.NoFontSelected)
  pure $
    Box.CharBox
      Box.Box
        { contents = char,
          boxWidth = width ^. typed @Q.Length,
          boxHeight = height ^. typed @Q.Length,
          boxDepth = depth ^. typed @Q.Length
        }

hModeStartParagraph ::
  ( HSt.EHexState :> es,
    Build.HListBuilder :> es
  ) =>
  ListExtractor.IndentFlag ->
  Eff es ()
hModeStartParagraph = \case
  ListExtractor.DoNotIndent ->
    pure ()
  -- \indent: An empty box of width \parindent is appended to the current
  -- list, and the space factor is set to 1000.
  -- TODO: Space factor.
  ListExtractor.Indent -> do
    HSt.getParIndentBox >>= Build.addHListElement
