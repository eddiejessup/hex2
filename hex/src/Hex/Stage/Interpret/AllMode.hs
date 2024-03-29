module Hex.Stage.Interpret.AllMode where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.HexIO.Interface qualified as HIO
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Group
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.Horizontal.Set qualified as Build.H.Set
import Hex.Stage.Build.ListBuilder.Interface qualified as Build
import Hex.Stage.Build.ListElem qualified as ListElem
import Hex.Stage.Build.ListExtractor.Interface (ExtractList)
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Build.Vertical.Set qualified as Build.V.Set
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as Eval
import Hex.Stage.Parse.Interface qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hexlude

data InterpretError
  = SawEndBoxInMainVMode
  | SawEndBoxInMainVModePara -- "No box to end: in paragraph within main V mode"
  | NoFontSelected
  | UnexpectedEndOfInput
  | VModeCommandInInnerHMode
  | CharacterCodeNotFound
  | BadOffset
  | UnboxWrongBoxAxis
  deriving stock (Show, Generic)

fmtInterpretError :: Fmt InterpretError
fmtInterpretError = F.shown

data AllModeCommandResult
  = SawEndBox
  | DidNotSeeEndBox

getNextCommandLogged ::
  ( Eval.HexEvaluate :> es,
    Par.CommandSource :> es,
    Error InterpretError :> es,
    Log.HexLog :> es
  ) =>
  Eff es Eval.Command
getNextCommandLogged = do
  cmd <- Eval.getEvalCommand >>= note UnexpectedEndOfInput
  Log.infoLog $ F.sformat ("Read command: " |%| F.shown) cmd
  pure cmd

data OutputDestination
  = FileStream Q.HexInt
  | StandardStream PT.StandardOutputInput

writeToOutput :: Monad m => OutputDestination -> Text -> m ()
writeToOutput _ _ = pure ()

-- let _handle = case stdStream of
--       PT.StdOut -> stdout
--       PT.StdErr -> stderr
-- liftIO $ hPutStrLn _handle expandedText
-- fStreams <- use $ typed @Config % #outFileStreams
-- let txtTxt = Codes.unsafeCodesAsChars (showExpandedBalancedText eTxt)
-- If stream number corresponds to an existing, open file:
--     write to file
-- Otherwise:
--     - Write to log
--     - If stream number is >= 0: also write to terminal
-- case getFileStream fStreams en of
--   Just fStream ->
--     liftIO $ hPutStrLn fStream txtTxt
--   Nothing -> do
-- -- Write to terminal.
-- when (en >= 0) $ sLog (BS.C8.pack txtTxt)
-- -- Write to log
-- logHandle <- use $ typed @Config % #logStream
-- liftIO $ hPutStrLn logHandle txtTxt

handleModeIndependentCommand ::
  forall es.
  ( HSt.EHexState :> es,
    Log.HexLog :> es,
    HIO.HexIO :> es,
    Build.HexListBuilder :> es,
    ListExtractor.ExtractList :> es,
    Error InterpretError :> es
  ) =>
  Eval.ModeIndependentCommand ->
  Eff es AllModeCommandResult
handleModeIndependentCommand = \case
  Eval.DebugShowState -> do
    Log.logInternalState Log.Info
    pure DidNotSeeEndBox
  Eval.WriteMessage (Eval.MessageWriteCommand stdStream expandedText) -> do
    writeToOutput (StandardStream stdStream) expandedText
    pure DidNotSeeEndBox
  Eval.WriteToStream (Eval.StreamWriteCommand n writeText) -> do
    case writeText of
      Eval.ImmediateWriteText txt -> writeToOutput (FileStream n) txt
      Eval.DeferredWriteText _ ->
        notImplemented "Write to stream: DeferredWriteText"
    pure DidNotSeeEndBox
  Eval.Relax ->
    pure DidNotSeeEndBox
  Eval.IgnoreSpaces ->
    pure DidNotSeeEndBox
  -- Re-insert the ⟨token⟩ into the input just after running the next
  -- assignment command. Later \afterassignment commands override earlier
  -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
  -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
  -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
  -- inserted by \everyhbox or \everyvbox.
  Eval.SetAfterAssignmentToken lt -> do
    HSt.setAfterAssignmentToken lt
    pure DidNotSeeEndBox
  Eval.AddPenalty p -> do
    Build.addVListElement $ ListElem.ListPenalty p
    pure DidNotSeeEndBox
  Eval.AddKern k -> do
    Build.addVListElement $ ListElem.VListBaseElem $ BoxElem.KernBaseElem k
    pure DidNotSeeEndBox
  Eval.Assign Eval.Assignment {Eval.scope, Eval.body} -> do
    case body of
      Eval.DefineControlSequence cs tgt -> do
        maySymbolTarget <- case tgt of
          Eval.MacroTarget macroDefinition -> pure $ Just $ RT.ExpansionCommandHeadToken $ ST.MacroTok macroDefinition
          Eval.LetTarget lexTokenTargetValue ->
            case lexTokenTargetValue of
              -- If the target of the \let is a char-cat pair, then resolve the
              -- new control sequence to a 'let-char-cat' containing that token.
              LT.CharCatLexToken charCat ->
                pure $ Just $ RT.PrimitiveToken $ PT.LetCharCat charCat
              -- If the target is a control sequence, then try to resolve that symbol.
              LT.ControlSequenceLexToken controlSequence ->
                HSt.resolveSymbol (Res.ControlSequenceSymbol controlSequence) >>= \case
                  -- If the target symbol does not exist, then do nothing, i.e.
                  -- assign no new control sequence. (I am basing this on the
                  -- behaviour of tex-the-program.)
                  Nothing -> pure Nothing
                  -- If the target symbol does exist, then create a new symbol
                  -- entry with the same contents as the target symbol.
                  Just symbol -> pure $ Just symbol
          Eval.FutureLetTarget _futureLetDefinition -> notImplemented "Define control sequence: future let"
          Eval.ShortDefineTarget shortDefTargetValue -> pure $ Just $ RT.PrimitiveToken $ PT.ShortDefTargetToken shortDefTargetValue
          Eval.ReadTarget _readInt -> notImplemented "ReadTarget"
          Eval.FontTarget (Eval.FontFileSpec fontSpec fontPath) -> do
            newFontNr <- HSt.loadFont fontPath fontSpec
            pure $ Just $ RT.PrimitiveToken $ PT.FontRefToken newFontNr
        case maySymbolTarget of
          Nothing ->
            pure ()
          Just symbolTarget ->
            HSt.setScopedValue (HSt.SymbolValue cs symbolTarget) scope
      Eval.AssignCode (Eval.CodeAssignment idxChar codeVal) ->
        let scopedVal = case codeVal of
              Eval.CatCodeValue catCode ->
                HSt.HexCodeValue Code.CCatCodeType idxChar catCode
              Eval.MathCodeValue mathCode ->
                HSt.HexCodeValue Code.CMathCodeType idxChar mathCode
              Eval.UpperCaseCodeValue upperCaseCode ->
                HSt.HexCodeValue Code.CUpperCaseCodeType idxChar upperCaseCode
              Eval.LowerCaseCodeValue lowerCaseCode ->
                HSt.HexCodeValue Code.CLowerCaseCodeType idxChar lowerCaseCode
              Eval.SpaceFactorCodeValue spaceFactorCode ->
                HSt.HexCodeValue Code.CSpaceFactorCodeType idxChar spaceFactorCode
              Eval.DelimiterCodeValue delimiterCode ->
                HSt.HexCodeValue Code.CDelimiterCodeType idxChar delimiterCode
         in HSt.setScopedValue scopedVal scope
      Eval.SetVariable ass ->
        case ass of
          Eval.IntVariableAssignment varAssignment ->
            setQuantVariable varAssignment scope
          Eval.LengthVariableAssignment varAssignment ->
            setQuantVariable varAssignment scope
          Eval.GlueVariableAssignment varAssignment ->
            setQuantVariable varAssignment scope
          Eval.MathGlueVariableAssignment varAssignment ->
            setQuantVariable varAssignment scope
          Eval.TokenListVariableAssignment varAssignment ->
            setQuantVariable varAssignment scope
          Eval.SpecialIntParameterVariableAssignment param tgt ->
            HSt.setSpecialIntParameter param tgt
          Eval.SpecialLengthParameterVariableAssignment param tgt ->
            HSt.setSpecialLengthParameter param tgt
      Eval.ModifyVariable modCommand ->
        case modCommand of
          Eval.AdvanceIntVariable var plusVal ->
            advanceQuantVariable var plusVal scope
          Eval.AdvanceLengthVariable var plusVal ->
            advanceQuantVariable var plusVal scope
          Eval.AdvanceGlueVariable var plusVal ->
            advanceQuantVariable var plusVal scope
          Eval.AdvanceMathGlueVariable var plusVal ->
            advanceQuantVariable var plusVal scope
          Eval.ScaleVariable scaleDirection numVar scaleVal ->
            case numVar of
              Eval.IntNumericVariable var ->
                scaleQuantVariable var scaleDirection scaleVal scope
              Eval.LengthNumericVariable var ->
                scaleQuantVariable var scaleDirection scaleVal scope
              Eval.GlueNumericVariable var ->
                scaleQuantVariable var scaleDirection scaleVal scope
              Eval.MathGlueNumericVariable var ->
                scaleQuantVariable var scaleDirection scaleVal scope
      Eval.SelectFont fNr -> selectFontInternallyAndDVI fNr scope
      Eval.SetFamilyMember familyMember fontNumber ->
        HSt.setScopedValue (HSt.FamilyMemberFontValue familyMember fontNumber) scope
      -- Start a new level of grouping. Enter inner mode.
      Eval.SetBoxRegister lhsIdx box -> do
        mayBox <- fetchBox box
        HSt.setScopedValue (HSt.BoxRegisterValue lhsIdx mayBox) scope
      Eval.SetFontSpecialChar (Eval.FontSpecialCharRef fontSpecialChar fontNr) charRef ->
        HSt.setFontSpecialCharacter fontSpecialChar fontNr charRef
      Eval.SetParShape _int _lengths ->
        notImplemented "Assignment body: SetParShape"
      Eval.SetFontDimension _fontDimensionRef _length ->
        notImplemented "Assignment body: SetFontDimension"
      Eval.SetHyphenation hyphenationExceptions ->
        HSt.setHyphenationExceptions hyphenationExceptions
      Eval.SetHyphenationPatterns hyphenationPatterns ->
        HSt.setHyphenationPatterns hyphenationPatterns
      Eval.SetBoxDimension _boxDimensionRef _length ->
        notImplemented "Assignment body: SetBoxDimension"
      Eval.SetInteractionMode _interactionMode ->
        notImplemented "Assignment body: SetInteractionMode"
    -- Now that we're done with an assignment, see whether any
    -- 'after-assignment' token was set.
    HSt.popAfterAssignmentToken >>= \case
      Nothing -> pure ()
      -- If a token was indeed set, put it into the input.
      Just lt -> HIO.insertLexToken lt
    pure DidNotSeeEndBox
  -- Start a new level of grouping.
  Eval.ChangeScope Q.Positive entryTrigger -> do
    HSt.pushGroup (Just (HSt.Group.LocalStructureScopeGroup entryTrigger))
    pure DidNotSeeEndBox
  -- Do the appropriate finishing actions, undo the
  -- effects of non-global assignments, and leave the
  -- group. Maybe leave the current mode.
  Eval.ChangeScope Q.Negative exitTrigger ->
    popGroupWithElems exitTrigger >>= \case
      HSt.Grouped.LocalStructureGroupType ->
        pure DidNotSeeEndBox
      HSt.Grouped.ExplicitBoxGroupType ->
        pure SawEndBox
      HSt.Grouped.NonScopeGroupType ->
        notImplemented "ChangeScope Negative: NonScopeGroupType"
  Eval.AddBox boxPlacement box -> do
    fetchBox box >>= \case
      Nothing -> pure ()
      Just b -> do
        -- We have 'shift' commands:
        -- - \moveleft, \moveright: can be used only on vboxes
        -- - \raise, \lower: can be used only on hboxes
        -- We couldn't have known until fetching the box whether the command
        -- used is appropriate for the box type, so check that now.
        offset <- case boxPlacement of
          Nothing ->
            pure Nothing
          Just (P.OffsetAlongAxis ax offsetInDirection) ->
            -- Confusingly, if we are building a *h*-box, we need to be asked to offset in the *v* direction.
            -- so we actually want to test for inequality here.
            if ax == BoxElem.axBoxElemsAxis b.boxedContents
              then throwError BadOffset
              else pure $ Just offsetInDirection

        -- If this box is offset, we need to do two things:
        -- (1) We must record the offset in the element we append to our list,
        --     so that when we render the page, we know to move up or down the
        --     page within the box.
        -- (2) We must change the dimensions of the box itself, so that we
        --     understand its true bounding box. To do this, we must inspect the
        --     effective offset direction to see what to do:
        --     (2a) If the effective movement is up the page, or 'backwards' in the
        --          generic language of the offsetable type, then we should:
        --          (2a, i)  Increase the height by this amount
        --          (2b, ii) Decrease the depth.
        --     (2b) If the effective movement is down the page, or 'forwards', then we should:
        --          (2a, i)  Decrease the height by this amount
        --          (2b, ii) Increase the depth.
        -- TODO: Should we floor height and depth at zero?
        let bWithOffsetDims = case offset of
              Nothing -> b
              Just realOffset ->
                let offsetDownPage = Box.offsetForward realOffset

                    heightOffset =
                      if offsetDownPage >= Q.zeroLength
                        then offsetDownPage
                        else invert offsetDownPage
                 in b
                      & #boxedDims % #boxHeight %~ (<> heightOffset)
                      & #boxedDims % #boxDepth %~ (<> invert heightOffset)
        when (bWithOffsetDims /= b) $
          Log.debugLog $
            F.sformat ("b before offset: " |%| Box.fmtBoxDimens |%| "; b after offset: " |%| Box.fmtBoxDimens) b.boxedDims bWithOffsetDims.boxedDims

        Build.addVListElement $
          ListElem.VListBaseElem $
            BoxElem.AxOrRuleBoxBaseElem $
              bWithOffsetDims <&> \axBoxElems ->
                BoxElem.AxBoxOrRuleContentsAx $
                  Box.Offsettable
                    { offset,
                      offsetContents = axBoxElems
                    }
    pure DidNotSeeEndBox
  Eval.AddMathKern _mathLength ->
    notImplemented "handleModeIndependentCommand: AddMathKern"
  Eval.RemoveItem _removableItem ->
    notImplemented "handleModeIndependentCommand: RemoveItem"
  Eval.AddToAfterGroupTokens _lexToken ->
    notImplemented "handleModeIndependentCommand: AddToAfterGroupTokens"
  Eval.ModifyFileStream fileStreamModificationCommand -> do
    case (fileStreamModificationCommand.fileStreamType, fileStreamModificationCommand.fileStreamAction) of
      (P.FileInput, P.Open path) -> HIO.openStreamFile path fileStreamModificationCommand.fileStreamNr HIO.InputFile
      (P.FileOutput P.Immediate, P.Open path) -> HIO.openStreamFile path fileStreamModificationCommand.fileStreamNr HIO.OutputFile
      (P.FileOutput P.Deferred, P.Open _path) -> notImplemented "handleModeIndependentCommand: Open output file, deferred"
      (P.FileInput, P.Close) -> notImplemented "handleModeIndependentCommand: Close input file"
      (P.FileOutput _writePolicy, P.Close) -> notImplemented "handleModeIndependentCommand: Close output file"
    pure DidNotSeeEndBox
  Eval.DoSpecial _text ->
    notImplemented "handleModeIndependentCommand: DoSpecial"
  where
    advanceQuantVariable var plusVal scope = case var of
      HSt.Var.ParamVar param ->
        HSt.advanceParameterValue param plusVal scope
      HSt.Var.RegisterVar registerLoc ->
        HSt.advanceRegisterValue registerLoc plusVal scope

    scaleQuantVariable var scaleDirection scaleVal scope = case var of
      HSt.Var.ParamVar param ->
        HSt.scaleParameterValue param scaleDirection scaleVal scope
      HSt.Var.RegisterVar registerLoc ->
        HSt.scaleRegisterValue registerLoc scaleDirection scaleVal scope

    setQuantVariable (Eval.QuantVariableAssignment var tgt) scope =
      case var of
        HSt.Var.ParamVar param ->
          HSt.setScopedValue (HSt.ParameterValue param tgt) scope
        HSt.Var.RegisterVar registerLoc ->
          HSt.setScopedValue (HSt.QuantRegisterValue registerLoc tgt) scope

    fetchBox :: Eval.Box -> Eff es (Maybe (Box.Boxed BoxElem.AxBoxElems))
    fetchBox = \case
      Eval.FetchedRegisterBox fetchMode rhsIdx -> do
        HSt.fetchBoxRegisterValue fetchMode rhsIdx
      Eval.LastBox ->
        notImplemented "Fetch LastBox"
      Eval.VSplitBox _ _ ->
        notImplemented "Fetch VSplitBox"
      Eval.ExplicitBox spec boxType -> do
        Just <$> fetchExplicitBox spec boxType

    fetchExplicitBox spec boxType = do
      HSt.pushGroup (Just HSt.Group.ExplicitBoxScopeGroup)
      Log.debugLog "Extracting explicit box"
      extractedBox <- extractExplicitBox spec boxType
      Log.debugLog $ F.sformat ("Extracted explicit box: " |%| BoxElem.fmtBoxedAxBoxElemsOneLine) extractedBox
      pure extractedBox

    popGroupWithElems exitTrigger = do
      HSt.popGroup exitTrigger

    selectFontInternallyAndDVI fNr = HSt.setScopedValue (HSt.FontValue fNr)

lengthToSetAtFromSpec :: Eval.BoxSpecification -> Q.Length -> Q.Length
lengthToSetAtFromSpec spec naturalLength = case spec of
  Eval.Natural -> naturalLength
  Eval.To toLength -> toLength
  Eval.Spread spreadLength -> naturalLength <> spreadLength

extractExplicitBox :: ExtractList :> es => Eval.BoxSpecification -> PT.ExplicitBoxType -> Eff es (Box.Boxed BoxElem.AxBoxElems)
extractExplicitBox spec = \case
  PT.ExplicitHBoxType -> do
    hList <- ListExtractor.extractHBoxList
    let naturalWidth = ListElem.hListNaturalWidth hList
        naturalHeight = ListElem.hListNaturalHeight hList
        naturalDepth = ListElem.hListNaturalDepth hList
        widthToSetAt = lengthToSetAtFromSpec spec naturalWidth

        (hBoxElems, _) = Build.H.Set.setList hList widthToSetAt
    pure
      Box.Boxed
        { boxedContents = BoxElem.AxBoxElemsH hBoxElems,
          boxedDims =
            Box.BoxDims
              { boxWidth = widthToSetAt,
                boxHeight = naturalHeight,
                boxDepth = naturalDepth
              }
        }
  PT.ExplicitVBoxType vAlignType -> do
    vList <- ListExtractor.extractVBoxList
    let naturalWidth = ListElem.vListNaturalWidth vList
        naturalHeight = ListElem.vListNaturalHeight vList
        naturalDepth = ListElem.vListNaturalDepth vList
    pure $ case vAlignType of
      PT.DefaultAlign ->
        let heightToSetAt = lengthToSetAtFromSpec spec naturalHeight
            (vBoxElems, _) = Build.V.Set.setList vList heightToSetAt
         in Box.Boxed
              { boxedContents = BoxElem.AxBoxElemsV vBoxElems,
                boxedDims =
                  Box.BoxDims
                    { boxWidth = naturalWidth,
                      boxHeight = heightToSetAt,
                      boxDepth = naturalDepth
                    }
              }
      PT.TopAlign ->
        notImplemented "extractExplicitBox.ExplicitVBoxType.{TopAlign}"
