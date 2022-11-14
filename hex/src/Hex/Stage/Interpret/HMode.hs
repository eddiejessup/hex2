module Hex.Stage.Interpret.HMode where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.HexIO.Interface qualified as HIO
import Hex.Common.HexIO.Interface.CharSourceStack (CharSourceStack)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
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
import Hexlude

data HModeCommandResult
  = ContinueHMode
  | EndHList ListExtractor.EndHListReason
  deriving stock (Show, Generic)

handleCommandInHMode ::
  ( HSt.EHexState :> es,
    HIO.HexIO :> es,
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
      HIO.putInput oldSrc
      HIO.insertLexToken LT.parToken
      pure ContinueHMode
    HSt.Mode.InnerModeVariant -> do
      throwError AllMode.VModeCommandInInnerHMode
  Eval.HModeCommand hModeCommand -> case hModeCommand of
    Eval.AddHGlue g -> do
      addGlue g
      pure ContinueHMode
    Eval.AddCharacter c -> do
      fNr <- HSt.currentFontNumber
      HSt.charAsBox fNr c >>= \case
        Nothing -> throwError AllMode.CharacterCodeNotFound
        Just charBox ->
          Build.addHListElement $ ListElem.HListHBaseElem $ BoxElem.CharBoxHBaseElem charBox
      updateSpaceFactor c
      pure ContinueHMode
    Eval.AddHRule rule -> do
      Build.addHListElement $
        ListElem.HVListElem $
          ListElem.VListBaseElem $
            BoxElem.AxOrRuleBoxBaseElem $
              Box.Boxed
                { boxedDims = rule,
                  boxedContents = BoxElem.AxBoxOrRuleContentsRule
                }
      pure ContinueHMode
    Eval.AddControlSpace -> do
      HSt.currentFontNumber
        >>= HSt.controlSpaceGlue
        >>= addGlue
      pure ContinueHMode
    Eval.AddAccentedCharacter _n _assignments _mayCharCodeRef ->
      notImplemented "handleCommandInHMode: AddAccentedCharacter"
    Eval.AddItalicCorrection -> do
      Build.getLastHListElement >>= \case
        Nothing -> pure ()
        Just e -> case e of
          ListElem.HVListElem _vListElem ->
            pure ()
          ListElem.DiscretionaryItemElem _discrItem ->
            pure ()
          ListElem.HListHBaseElem hBaseElem -> case hBaseElem of
            -- TODO: Handle ligatures too.
            BoxElem.CharBoxHBaseElem charBox -> do
              let charCode = charBox.boxedContents.charBoxCharCode
              fNr <- HSt.currentFontNumber
              HSt.fontCharacter fNr charCode >>= \case
                -- No font selected
                Nothing -> pure ()
                Just charAttrs ->
                  Build.addHListElement $
                    ListElem.HVListElem $
                      ListElem.VListBaseElem $
                        BoxElem.KernBaseElem $
                          BoxElem.Kern charAttrs.italicCorrection
      pure ContinueHMode
    Eval.AddDiscretionaryText _discretionaryText ->
      notImplemented "handleCommandInHMode: AddDiscretionaryText"
    Eval.AddDiscretionaryHyphen -> do
      fNr <- HSt.currentFontNumber
      HSt.fontDiscretionaryHyphenItem fNr >>= \case
        Nothing -> pure ()
        Just item -> do
          Log.debugLog $ F.sformat ("Adding discretionary hyphen: " |%| ListElem.fmtDiscretionaryItem) item
          Build.addHListElement (ListElem.DiscretionaryItemElem item)
      pure ContinueHMode
    Eval.EnterMathMode ->
      notImplemented "handleCommandInHMode: EnterMathMode"
    Eval.AddHLeaders _leadersSpec ->
      notImplemented "handleCommandInHMode: AddHLeaders"
    Eval.AddUnwrappedFetchedHBox (Eval.FetchedBoxRef regLoc fetchMode) -> do
      HSt.fetchBoxRegisterValue fetchMode regLoc >>= \case
        Nothing -> pure ()
        Just b -> case b.boxedContents of
          BoxElem.AxBoxElemsH hboxElems ->
            Build.addUnboxedHListElements $ ListElem.hBoxElemAsHListElem <$> hboxElems
          BoxElem.AxBoxElemsV _vboxElems ->
            throwError AllMode.UnboxWrongBoxAxis
      pure ContinueHMode
    Eval.AddVAlignedMaterial _boxSpec ->
      notImplemented "handleCommandInHMode: HMode: AddVAlignedMaterial"
  Eval.ModeDependentCommand modeDependentCommand -> case modeDependentCommand of
    Eval.AddSpace -> do
      HSt.currentFontNumber
        >>= HSt.spaceGlue
        >>= addGlue
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
    Eval.ShowToken _lexToken -> notImplemented "handleCommandInHMode: HMode: ShowToken"
    Eval.ShowBox _n -> notImplemented "handleCommandInHMode: HMode: ShowBox"
    Eval.ShowLists -> notImplemented "handleCommandInHMode: HMode: ShowLists"
    Eval.ShowTheInternalQuantity _internalQuantity -> notImplemented "handleCommandInHMode: HMode: ShowTheInternalQuantity"
    Eval.ShipOut _box -> notImplemented "handleCommandInHMode: HMode: ShipOut"
    Eval.AddMark _text -> notImplemented "handleCommandInHMode: HMode: AddMark"
    Eval.AddInsertion _n -> notImplemented "handleCommandInHMode: HMode: AddInsertion"
    Eval.AddAdjustment -> notImplemented "handleCommandInHMode: HMode: AddAdjustment"
  Eval.ModeIndependentCommand modeIndependentCommand ->
    AllMode.handleModeIndependentCommand modeIndependentCommand <&> \case
      AllMode.SawEndBox ->
        EndHList ListExtractor.EndHListSawEndParaCommand
      AllMode.DidNotSeeEndBox ->
        ContinueHMode
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
    parDims <- HSt.getParIndentBoxDims
    Build.addHListElement (HSt.emptyHBoxAsHListElem parDims)
