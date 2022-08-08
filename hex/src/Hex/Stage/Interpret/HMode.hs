module Hex.Stage.Interpret.HMode where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Impl.Font qualified as HSt.Font
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Mode qualified as HSt.Mode
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
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
    ListExtractor.ExtractList :> es
  ) =>
  CharSourceStack ->
  HSt.Mode.ModeVariant ->
  Eval.Command ->
  Eff es HModeCommandResult
handleCommandInHMode oldSrc modeVariant = \case
  Eval.VModeCommand _ -> case modeVariant of
    HSt.Mode.OuterModeVariant -> do
      -- Insert the control sequence "\par" into the input. The control
      -- sequence's current meaning will be used, which might no longer be the \par
      -- primitive.
      HIn.putInput oldSrc
      HIn.insertLexToken LT.parToken
      pure ContinueHMode
    HSt.Mode.InnerModeVariant -> do
      throwError $ AllMode.VModeCommandInInnerHMode
  Eval.HModeCommand hModeCommand -> case hModeCommand of
    Eval.AddHGlue g -> do
      addGlue g
      pure ContinueHMode
    Eval.AddCharacter c -> do
      charBox <- charAsBox c
      Build.addHListElement $ ListElem.HListHBaseElem $ BoxElem.ElemCharacter charBox
      updateSpaceFactor c
      pure ContinueHMode
    Eval.AddHRule rule -> do
      Build.addHListElement $ ListElem.HVListElem $ ListElem.VListBaseElem $ BoxElem.ElemBox $ BoxElem.ruleAsBaseBox rule
      pure ContinueHMode
    Eval.AddControlSpace -> do
      HSt.currentControlSpaceGlue >>= addGlue
      pure ContinueHMode
    Eval.AddAccentedCharacter _n _assignments _mayCharCodeRef ->
      notImplemented "AddAccentedCharacter"
    Eval.AddItalicCorrection -> do
      Build.getLastHListElement >>= \case
        Nothing -> pure ()
        Just e -> case e of
          ListElem.HVListElem _vListElem ->
            pure ()
          ListElem.HListHBaseElem hBaseElem -> case hBaseElem of
            -- TODO: Handle ligatures too.
            BoxElem.ElemCharacter charBox -> do
              let charCode = charBox.unCharacter.contents
              HSt.currentFontCharacter charCode >>= \case
                -- No font selected
                Nothing -> pure ()
                Just charAttrs ->
                  Build.addHListElement $
                    ListElem.HVListElem $
                      ListElem.VListBaseElem $
                        BoxElem.ElemKern $ BoxElem.Kern charAttrs.italicCorrection
      pure ContinueHMode
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
    Eval.AddVAlignedMaterial _boxSpec ->
      notImplemented "HMode: AddVAlignedMaterial"
  Eval.AddSpace -> do
    HSt.currentSpaceGlue >>= addGlue
    pure ContinueHMode
  Eval.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag
    pure ContinueHMode
  -- \par: Restricted: does nothing. Unrestricted: ends mode.
  Eval.EndParagraph -> pure $ case modeVariant of
    HSt.Mode.OuterModeVariant ->
      EndHList ListExtractor.EndHListSawEndParaCommand
    HSt.Mode.InnerModeVariant ->
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
  Eval.AddInsertion _n -> notImplemented "HMode: AddInsertion"
  Eval.AddAdjustment -> notImplemented "HMode: AddAdjustment"
  where
    -- Tex by topic, §20.2, p186.
    updateSpaceFactor c = do
      lastCharSpaceFactorInt <- (.unSpaceFactorCode) <$> HSt.getHexCode Code.CSpaceFactorCodeType c
      unless (lastCharSpaceFactorInt == Q.zeroInt) $ do
        currentSpaceFactorInt <- HSt.getSpecialIntParameter HSt.Param.SpaceFactor
        let newSpaceFactor =
              if (lastCharSpaceFactorInt > Q.thousandInt && currentSpaceFactorInt < Q.thousandInt)
                then Q.thousandInt
                else lastCharSpaceFactorInt
        HSt.setSpecialIntParameter HSt.Param.SpaceFactor newSpaceFactor

    addGlue g = Build.addHListElement $ ListElem.HVListElem $ ListElem.ListGlue g

charAsBox ::
  ( HSt.EHexState :> es,
    Error AllMode.InterpretError :> es
  ) =>
  Codes.CharCode ->
  Eff es Box.CharBox
charAsBox char = do
  HSt.Font.CharacterAttrs {width, height, depth} <-
    HSt.currentFontCharacter char
      >>= note (AllMode.NoFontSelected)
  pure $
    Box.CharBox
      Box.Box
        { contents = char,
          boxWidth = width,
          boxHeight = height,
          boxDepth = depth
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
