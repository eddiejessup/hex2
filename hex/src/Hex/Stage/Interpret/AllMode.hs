module Hex.Stage.Interpret.AllMode where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
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
import Hex.Stage.Evaluate.Interface qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as Eval
import Hex.Stage.Parse.Interface qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hex.Stage.Read.Interface qualified as HIn
import Hex.Stage.Render.Interface.DocInstruction qualified as DVI
import Hexlude

data InterpretError
  = SawEndBoxInMainVMode
  | SawEndBoxInMainVModePara -- "No box to end: in paragraph within main V mode"
  | NoFontSelected
  | UnexpectedEndOfInput
  | VModeCommandInInnerHMode
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
  ( HSt.EHexState :> es,
    Log.HexLog :> es,
    HIn.HexInput :> es,
    Build.HexListBuilder :> es,
    ListExtractor.ExtractList :> es
  ) =>
  Eval.ModeIndependentCommand ->
  Eff es AllModeCommandResult
handleModeIndependentCommand = \case
  Eval.DebugShowState -> do
    Log.logInternalState
    pure DidNotSeeEndBox
  Eval.WriteMessage (Eval.MessageWriteCommand stdStream expandedText) -> do
    writeToOutput (StandardStream stdStream) expandedText
    pure DidNotSeeEndBox
  Eval.WriteToStream (Eval.StreamWriteCommand n writeText) -> do
    case writeText of
      Eval.ImmediateWriteText txt -> do
        writeToOutput (FileStream n) txt
      Eval.DeferredWriteText _ ->
        notImplemented "Write to stream: DeferredWriteText"
    pure DidNotSeeEndBox
  Eval.Relax ->
    pure DidNotSeeEndBox
  Eval.IgnoreSpaces ->
    pure DidNotSeeEndBox
  -- Re-insert the ???token??? into the input just after running the next
  -- assignment command. Later \afterassignment commands override earlier
  -- commands. If the assignment is a \setbox, and if the assigned ???box??? is
  -- \{hbox,vbox,vtop}, insert the ???token??? just after the '{' in the box
  -- construction (not after the '}'). Insert the ???token??? just before tokens
  -- inserted by \everyhbox or \everyvbox.
  Eval.SetAfterAssignmentToken lt -> do
    HSt.setAfterAssignmentToken lt
    pure DidNotSeeEndBox
  Eval.AddPenalty p -> do
    Build.addVListElement $ ListElem.ListPenalty p
    pure DidNotSeeEndBox
  Eval.AddKern k -> do
    Build.addVListElement $ ListElem.VListBaseElem $ BoxElem.ElemKern k
    pure DidNotSeeEndBox
  Eval.Assign Eval.Assignment {Eval.scope, Eval.body} -> do
    case body of
      Eval.DefineControlSequence cs tgt -> do
        maySymbolTarget <- case tgt of
          Eval.MacroTarget macroDefinition -> do
            pure $ Just $ RT.ExpansionCommandHeadToken $ ST.MacroTok macroDefinition
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
          Eval.FutureLetTarget _futureLetDefinition -> do
            notImplemented "Define control sequence: future let"
          Eval.ShortDefineTarget shortDefTargetValue -> do
            pure $ Just $ RT.PrimitiveToken $ PT.ShortDefTargetToken shortDefTargetValue
          Eval.ReadTarget _readInt -> do
            notImplemented "ReadTarget"
          Eval.FontTarget (Eval.FontFileSpec fontSpec fontPath) -> do
            fontDefinition <- HSt.loadFont fontPath fontSpec
            Build.addVListElement $ ListElem.VListBaseElem $ BoxElem.ElemFontDefinition fontDefinition
            pure $ Just $ RT.PrimitiveToken $ PT.FontRefToken fontDefinition.fontNr
        case maySymbolTarget of
          Nothing ->
            pure ()
          Just symbolTarget ->
            HSt.setSymbol cs symbolTarget scope
      Eval.AssignCode (Eval.CodeAssignment idxChar codeVal) ->
        case codeVal of
          Eval.CatCodeValue catCode ->
            HSt.setHexCode Code.CCatCodeType idxChar catCode scope
          Eval.MathCodeValue mathCode ->
            HSt.setHexCode Code.CMathCodeType idxChar mathCode scope
          Eval.UpperCaseCodeValue upperCaseCode ->
            HSt.setHexCode Code.CUpperCaseCodeType idxChar upperCaseCode scope
          Eval.LowerCaseCodeValue lowerCaseCode ->
            HSt.setHexCode Code.CLowerCaseCodeType idxChar lowerCaseCode scope
          Eval.SpaceFactorCodeValue spaceFactorCode ->
            HSt.setHexCode Code.CSpaceFactorCodeType idxChar spaceFactorCode scope
          Eval.DelimiterCodeValue delimiterCode ->
            HSt.setHexCode Code.CDelimiterCodeType idxChar delimiterCode scope
      Eval.SetVariable ass ->
        case ass of
          Eval.IntVariableAssignment (Eval.QuantVariableAssignment var tgt) ->
            case var of
              HSt.Var.ParamVar intParam ->
                HSt.setParameterValue intParam tgt scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.setQuantRegisterValue registerLoc tgt scope
          Eval.LengthVariableAssignment (Eval.QuantVariableAssignment var tgt) ->
            case var of
              HSt.Var.ParamVar lengthParam ->
                HSt.setParameterValue lengthParam tgt scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.setQuantRegisterValue registerLoc tgt scope
          Eval.GlueVariableAssignment (Eval.QuantVariableAssignment var tgt) ->
            case var of
              HSt.Var.ParamVar glueParam ->
                HSt.setParameterValue glueParam tgt scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.setQuantRegisterValue registerLoc tgt scope
          Eval.MathGlueVariableAssignment (Eval.QuantVariableAssignment var tgt) ->
            case var of
              HSt.Var.ParamVar mathGlueParam ->
                HSt.setParameterValue mathGlueParam tgt scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.setQuantRegisterValue registerLoc tgt scope
          Eval.TokenListVariableAssignment (Eval.QuantVariableAssignment var tgt) ->
            case var of
              HSt.Var.ParamVar tokenListParam ->
                HSt.setParameterValue tokenListParam tgt scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.setQuantRegisterValue registerLoc tgt scope
          Eval.SpecialIntParameterVariableAssignment param tgt ->
            HSt.setSpecialIntParameter param tgt
          Eval.SpecialLengthParameterVariableAssignment param tgt ->
            HSt.setSpecialLengthParameter param tgt
      Eval.ModifyVariable modCommand ->
        case modCommand of
          Eval.AdvanceIntVariable var plusVal ->
            case var of
              HSt.Var.ParamVar param ->
                HSt.advanceParameterValue param plusVal scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.advanceRegisterValue registerLoc plusVal scope
          Eval.AdvanceLengthVariable var plusVal ->
            case var of
              HSt.Var.ParamVar param ->
                HSt.advanceParameterValue param plusVal scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.advanceRegisterValue registerLoc plusVal scope
          Eval.AdvanceGlueVariable var plusVal ->
            case var of
              HSt.Var.ParamVar param ->
                HSt.advanceParameterValue param plusVal scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.advanceRegisterValue registerLoc plusVal scope
          Eval.AdvanceMathGlueVariable var plusVal ->
            case var of
              HSt.Var.ParamVar param ->
                HSt.advanceParameterValue param plusVal scope
              HSt.Var.RegisterVar registerLoc ->
                HSt.advanceRegisterValue registerLoc plusVal scope
          Eval.ScaleVariable scaleDirection numVar scaleVal ->
            case numVar of
              Eval.IntNumericVariable var ->
                case var of
                  HSt.Var.ParamVar param ->
                    HSt.scaleParameterValue param scaleDirection scaleVal scope
                  HSt.Var.RegisterVar registerLoc ->
                    HSt.scaleRegisterValue registerLoc scaleDirection scaleVal scope
              Eval.LengthNumericVariable var ->
                case var of
                  HSt.Var.ParamVar param ->
                    HSt.scaleParameterValue param scaleDirection scaleVal scope
                  HSt.Var.RegisterVar registerLoc ->
                    HSt.scaleRegisterValue registerLoc scaleDirection scaleVal scope
              Eval.GlueNumericVariable var ->
                case var of
                  HSt.Var.ParamVar param ->
                    HSt.scaleParameterValue param scaleDirection scaleVal scope
                  HSt.Var.RegisterVar registerLoc ->
                    HSt.scaleRegisterValue registerLoc scaleDirection scaleVal scope
              Eval.MathGlueNumericVariable var ->
                case var of
                  HSt.Var.ParamVar param ->
                    HSt.scaleParameterValue param scaleDirection scaleVal scope
                  HSt.Var.RegisterVar registerLoc ->
                    HSt.scaleRegisterValue registerLoc scaleDirection scaleVal scope
      Eval.SelectFont fNr -> do
        HSt.selectFont fNr scope
        Build.addVListElement $ ListElem.VListBaseElem $ BoxElem.ElemFontSelection fNr
      Eval.SetFamilyMember familyMember fontNumber ->
        HSt.setFamilyMemberFont familyMember fontNumber scope
      -- Start a new level of grouping. Enter inner mode.
      Eval.SetBoxRegister lhsIdx box -> do
        mayBox <- fetchBox box
        HSt.setBoxRegisterValue lhsIdx mayBox scope
      Eval.SetFontSpecialChar (Eval.FontSpecialCharRef fontSpecialChar fontNr) charRef ->
        HSt.setFontSpecialCharacter fontSpecialChar fontNr charRef
      assignment ->
        notImplemented $ "Assignment body: " <> show assignment
    -- Now that we're done with an assignment, see whether any
    -- 'after-assignment' token was set.
    HSt.popAfterAssignmentToken >>= \case
      Nothing -> pure ()
      -- If a token was indeed set, put it into the input.
      Just lt -> HIn.insertLexToken lt
    pure DidNotSeeEndBox
  -- Start a new level of grouping.
  Eval.ChangeScope Q.Positive entryTrigger -> do
    HSt.pushGroup (Just (HSt.Group.LocalStructureScopeGroup entryTrigger))
    pure DidNotSeeEndBox
  -- Do the appropriate finishing actions, undo the
  -- effects of non-global assignments, and leave the
  -- group. Maybe leave the current mode.
  Eval.ChangeScope Q.Negative exitTrigger ->
    --   prePopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
    --   postPopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
    --   when (prePopCurrentFontNr /= postPopCurrentFontNr) $ do
    --     Build.addVListElement $ ListElem.VListBaseElem $ BoxElem.ElemFontSelection $ BoxElem.FontSelection (fromMaybe 0 postPopCurrentFontNr)
    HSt.popGroup exitTrigger >>= \case
      HSt.Grouped.LocalStructureGroupType ->
        pure DidNotSeeEndBox
      HSt.Grouped.ExplicitBoxGroupType ->
        pure SawEndBox
      HSt.Grouped.NonScopeGroupType ->
        notImplemented "ChangeScope Negative: NonScopeGroupType"
  Eval.AddBox boxPlacement box -> do
    fetchBox box >>= \case
      Nothing -> pure ()
      Just b -> case boxPlacement of
        P.NaturalPlacement ->
          Build.addVListElement $ ListElem.VListBaseElem $ BoxElem.ElemBox b
        P.ShiftedPlacement _axis _direction _distance ->
          notImplemented $ "AddBox with ShiftedPlacement: " <> show boxPlacement
    pure DidNotSeeEndBox
  Eval.AddMathKern _mathLength ->
    notImplemented "handleModeIndependentCommand: AddMathKern"
  Eval.RemoveItem _removableItem ->
    notImplemented "handleModeIndependentCommand: RemoveItem"
  Eval.AddToAfterGroupTokens _lexToken ->
    notImplemented "handleModeIndependentCommand: AddToAfterGroupTokens"
  Eval.ModifyFileStream _fileStreamModificationCommand ->
    notImplemented "handleModeIndependentCommand: ModifyFileStream"
  Eval.DoSpecial _text ->
    notImplemented "handleModeIndependentCommand: DoSpecial"
  where
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
      Log.debugLog $ F.sformat ("Extracted explicit box: " |%| BoxElem.fmtBaseBox) extractedBox
      pure extractedBox

lengthToSetAtFromSpec :: Eval.BoxSpecification -> Q.Length -> Q.Length
lengthToSetAtFromSpec spec naturalLength = case spec of
  Eval.Natural -> naturalLength
  Eval.To toLength -> toLength
  Eval.Spread spreadLength -> naturalLength <> spreadLength

extractExplicitBox :: ExtractList :> es => Eval.BoxSpecification -> PT.ExplicitBoxType -> Eff es BoxElem.BaseBox
extractExplicitBox spec = \case
  PT.ExplicitHBoxType -> do
    hList <- ListExtractor.extractHBoxList
    let (naturalWidth, naturalDepth, naturalHeight) = ListElem.hListNaturalDimens hList
        widthToSetAt = lengthToSetAtFromSpec spec naturalWidth
        (hBoxElems, _) = Build.H.Set.setList hList widthToSetAt
        hBoxContents = BoxElem.HBoxContents hBoxElems
        hBox =
          BoxElem.BaseBox $
            Box.Box
              { contents = hBoxContents,
                boxWidth = widthToSetAt,
                boxHeight = naturalHeight,
                boxDepth = naturalDepth
              }
    pure hBox
  PT.ExplicitVBoxType _vAlignType -> do
    _vList <- ListExtractor.extractVBoxList
    notImplemented "extractExplicitBox: ExplicitVBoxType"
