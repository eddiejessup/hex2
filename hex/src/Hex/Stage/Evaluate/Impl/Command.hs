module Hex.Stage.Evaluate.Impl.Command where

import Data.List.NonEmpty qualified as L.NE
import Data.Map.Strict qualified as Map
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface (EHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Hyphen qualified as HSt.Hyph
import Hex.Common.TFM.Types qualified as TFM
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Evaluate.Impl.Common (EvaluationError (..))
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.Quantity qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as E
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as E
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hex.Stage.Parse.Interface.AST.Quantity qualified as P
import Hexlude
import Witherable qualified as Wither

evalCommand :: [Error Eval.EvaluationError, EHexState] :>> es => P.Command -> Eff es E.Command
evalCommand = \case
  P.ShowToken lt -> pure $ E.ShowToken lt
  P.ShowBox n -> E.ShowBox <$> Eval.evalInt n
  P.VModeCommand vModeCommand -> E.VModeCommand <$> evalVModeCommand vModeCommand
  P.HModeCommand hModeCommand -> E.HModeCommand <$> evalHModeCommand hModeCommand
  P.ModeIndependentCommand modeIndepCmd -> E.ModeIndependentCommand <$> evalModeIndepCmd modeIndepCmd
  P.ShowLists -> pure E.ShowLists
  P.ShowTheInternalQuantity internalQuantity -> pure $ E.ShowTheInternalQuantity internalQuantity
  P.ShipOut box -> pure $ E.ShipOut box
  P.AddMark expandedBalancedText -> pure $ E.AddMark expandedBalancedText
  P.AddInsertion n -> E.AddInsertion <$> Eval.evalInt n
  P.AddAdjustment -> pure $ E.AddAdjustment
  P.AddSpace -> pure E.AddSpace
  P.StartParagraph indentFlag -> pure $ E.StartParagraph indentFlag
  P.EndParagraph -> pure E.EndParagraph

evalVModeCommand :: [Error Eval.EvaluationError, EHexState] :>> es => P.VModeCommand -> Eff es E.VModeCommand
evalVModeCommand = \case
  P.End -> pure E.End
  P.Dump -> pure E.Dump
  P.EnterHMode -> pure E.EnterHMode
  P.AddVGlue glue -> E.AddVGlue <$> Eval.evalGlue glue
  P.AddVLeaders leadersSpec -> pure $ E.AddVLeaders leadersSpec
  P.AddVRule rule -> E.AddVRule <$> Eval.evalVModeRule rule
  P.AddHAlignedMaterial boxSpec -> E.AddHAlignedMaterial <$> evalBoxSpecification boxSpec
  P.AddUnwrappedFetchedVBox fetchedBoxRef -> pure $ E.AddUnwrappedFetchedVBox fetchedBoxRef

evalHModeCommand :: [Error Eval.EvaluationError, EHexState] :>> es => P.HModeCommand -> Eff es E.HModeCommand
evalHModeCommand = \case
  P.AddControlSpace -> pure E.AddControlSpace
  P.AddCharacter charCodeRef -> E.AddCharacter <$> evalCharCodeRef charCodeRef
  P.AddAccentedCharacter n assignments mayCharCodeRef -> do
    -- TODO: Can we do this? Do we need to actually execute the assignments in
    -- order? The previous assignment might affect a later assignment.
    eAssignments <- for assignments evalAssignment
    pure $ E.AddAccentedCharacter n eAssignments mayCharCodeRef
  P.AddItalicCorrection -> pure E.AddItalicCorrection
  P.AddDiscretionaryText discretionaryText -> pure $ E.AddDiscretionaryText discretionaryText
  P.AddDiscretionaryHyphen -> pure E.AddDiscretionaryHyphen
  P.EnterMathMode -> pure E.EnterMathMode
  P.AddHGlue glue -> E.AddHGlue <$> Eval.evalGlue glue
  P.AddHLeaders leadersSpec -> pure $ E.AddHLeaders leadersSpec
  P.AddHRule rule -> E.AddHRule <$> Eval.evalHModeRule rule
  P.AddVAlignedMaterial boxSpec -> E.AddVAlignedMaterial <$> evalBoxSpecification boxSpec
  P.AddUnwrappedFetchedHBox fetchedBoxRef -> pure $ E.AddUnwrappedFetchedHBox fetchedBoxRef

evalModeIndepCmd :: [Error Eval.EvaluationError, EHexState] :>> es => P.ModeIndependentCommand -> Eff es E.ModeIndependentCommand
evalModeIndepCmd = \case
  P.Assign assignment -> E.Assign <$> evalAssignment assignment
  P.Relax -> pure E.Relax
  P.IgnoreSpaces -> pure E.IgnoreSpaces
  P.AddPenalty hexInt -> E.AddPenalty . Bad.FiniteBadnessVal <$> Eval.evalInt hexInt
  P.AddKern length -> E.AddKern . BoxElem.Kern <$> Eval.evalLength length
  P.AddMathKern mathLength -> E.AddMathKern <$> Eval.evalMathLength mathLength
  P.RemoveItem removableItem -> pure $ E.RemoveItem removableItem
  P.SetAfterAssignmentToken lexToken -> pure $ E.SetAfterAssignmentToken lexToken
  P.AddToAfterGroupTokens lexToken -> pure $ E.AddToAfterGroupTokens lexToken
  P.WriteMessage messageWriteCommand -> E.WriteMessage <$> evalMessageWriteCommand messageWriteCommand
  P.ModifyFileStream fileStreamModificationCommand -> pure $ E.ModifyFileStream fileStreamModificationCommand
  P.WriteToStream streamWriteCommand -> E.WriteToStream <$> evalStreamWriteCommand streamWriteCommand
  P.DoSpecial expandedBalancedText -> pure $ E.DoSpecial expandedBalancedText
  P.AddBox boxPlacement box -> E.AddBox boxPlacement <$> evalBox box
  P.ChangeScope sign localStructureTrigger -> pure $ E.ChangeScope sign localStructureTrigger
  P.DebugShowState -> pure E.DebugShowState

evalCharCodeRef :: [Error Eval.EvaluationError, EHexState] :>> es => P.CharCodeRef -> Eff es Code.CharCode
evalCharCodeRef = \case
  P.CharRef charCode -> pure charCode
  P.CharTokenRef n -> Eval.noteRange @Code.CharCode n
  P.CharCodeNrRef charCodeInt -> Eval.evalCharCodeInt charCodeInt

evalAssignment :: [Error Eval.EvaluationError, EHexState] :>> es => P.Assignment -> Eff es E.Assignment
evalAssignment P.Assignment {body, scope} =
  E.Assignment <$> (evalAssignmentBody body) <*> pure scope

evalStreamWriteCommand :: [Error Eval.EvaluationError, EHexState] :>> es => P.StreamWriteCommand -> Eff es E.StreamWriteCommand
evalStreamWriteCommand (P.StreamWriteCommand n writeText) =
  E.StreamWriteCommand <$> Eval.evalInt n <*> evalWriteText writeText

evalWriteText :: Error Eval.EvaluationError :> es => P.WriteText -> Eff es E.WriteText
evalWriteText = \case
  P.ImmediateWriteText expandedBalancedText ->
    E.ImmediateWriteText <$> Eval.evalBalancedTextToText expandedBalancedText
  P.DeferredWriteText balancedText ->
    pure $ E.DeferredWriteText balancedText

evalAssignmentBody :: [Error Eval.EvaluationError, EHexState] :>> es => P.AssignmentBody -> Eff es E.AssignmentBody
evalAssignmentBody = \case
  P.DefineControlSequence controlSymbol controlSequenceTarget ->
    E.DefineControlSequence controlSymbol <$> evalControlSequenceTarget controlSequenceTarget
  P.SetVariable variableAssignment ->
    E.SetVariable <$> evalVariableAssignment variableAssignment
  P.ModifyVariable variableModification -> E.ModifyVariable <$> evalVariableModification variableModification
  P.AssignCode codeAssignment -> E.AssignCode <$> evalCodeAssignment codeAssignment
  P.SelectFont _fontNumber -> pure $ E.SelectFont $ _fontNumber
  P.SetFamilyMember familyMember fontRef -> E.SetFamilyMember <$> Eval.evalFamilyMember familyMember <*> Eval.evalFontRef fontRef
  P.SetParShape hexInt lengths -> pure $ E.SetParShape hexInt lengths
  P.SetBoxRegister loc box -> E.SetBoxRegister <$> (Eval.evalExplicitRegisterLocation loc) <*> evalBox box
  P.SetFontDimension fontDimensionRef length -> pure $ E.SetFontDimension fontDimensionRef length
  P.SetFontSpecialChar fontSpecialCharRef fontSpecialCharTgt -> E.SetFontSpecialChar <$> (evalFontSpecialCharRef fontSpecialCharRef) <*> Eval.evalInt fontSpecialCharTgt
  P.SetHyphenation hyphExceptions -> E.SetHyphenation <$> evalHyphenationExceptions hyphExceptions
  P.SetHyphenationPatterns patterns -> E.SetHyphenationPatterns <$> evalHyphenationPatterns patterns
  P.SetBoxDimension boxDimensionRef length -> pure $ E.SetBoxDimension boxDimensionRef length
  P.SetInteractionMode interactionMode -> pure $ E.SetInteractionMode interactionMode

evalHyphenationPatterns :: [Error Eval.EvaluationError, EHexState] :>> es => [HSt.Hyph.HyphenationPattern] -> Eff es [HSt.Hyph.HyphenationPattern]
evalHyphenationPatterns patterns = do
  validatedPatterns <- for patterns $ \(HSt.Hyph.HyphenationPattern preLast lastValue) -> do
    validatedPreLast <- for preLast $ \(value, letterCharCode) -> do
      validatedLetterCharCode <-
        -- Map a '.' into the zero char-code
        if letterCharCode == Code.Chr_ '.'
          then pure $ Code.CharCode 0
          else -- Otherwise, check whether the char-code has a zero lowercase
          -- code. If so, throw an error. Otherwise, use the lowercase code.
            toLowerCaseOrError letterCharCode
      pure (value, validatedLetterCharCode)
    pure $ HSt.Hyph.HyphenationPattern validatedPreLast lastValue
  pure $ validatedPatterns

evalHyphenationExceptions ::
  [Error Eval.EvaluationError, EHexState] :>> es =>
  [P.HyphenationException] ->
  Eff es (Map HSt.Hyph.WordCodes HSt.Hyph.WordHyphenationPoints)
evalHyphenationExceptions hyphExceptions =
  fmap Map.fromList $
    flip Wither.witherM hyphExceptions $ \(P.HyphenationException word) -> do
      codeWord <-
        Wither.catMaybes
          <$> for
            (toList word)
            ( \case
                Nothing -> pure Nothing
                Just letterCharCode -> Just <$> toLowerCaseOrError letterCharCode
            )

      let codePoints = HSt.Hyph.WordHyphenationPoints $
            flip Wither.mapMaybe (indexed (toList word)) $ \case
              (_, Just _) -> Nothing
              (i, Nothing) -> Just i

      pure $
        L.NE.nonEmpty codeWord <&> \neCodeWord ->
          (HSt.Hyph.WordCodes neCodeWord, codePoints)

toLowerCaseOrError :: [Error Eval.EvaluationError, EHexState] :>> es => Code.CharCode -> Eff es Code.CharCode
toLowerCaseOrError charCode =
  HSt.getHexCode Code.CLowerCaseCodeType charCode >>= \case
    Code.LowerCaseCode Code.NoCaseChange ->
      throwError $ InvalidLetterInHyphenationPatterns charCode
    Code.LowerCaseCode (Code.ChangeToCode lcCharCode) ->
      pure lcCharCode

evalFontSpecialCharRef :: EHexState :> es => P.FontSpecialCharRef -> Eff es E.FontSpecialCharRef
evalFontSpecialCharRef (P.FontSpecialCharRef fontSpecialChar fontNr) = E.FontSpecialCharRef fontSpecialChar <$> Eval.evalFontRef fontNr

evalBox :: [Error Eval.EvaluationError, EHexState] :>> es => P.Box -> Eff es E.Box
evalBox = \case
  P.FetchedRegisterBox boxFetchMode loc -> E.FetchedRegisterBox boxFetchMode <$> Eval.evalExplicitRegisterLocation loc
  P.LastBox -> pure E.LastBox
  P.VSplitBox n length -> E.VSplitBox <$> Eval.evalInt n <*> Eval.evalLength length
  P.ExplicitBox boxSpecification explicitBoxType -> E.ExplicitBox <$> evalBoxSpecification boxSpecification <*> pure explicitBoxType

evalBoxSpecification :: [Error Eval.EvaluationError, EHexState] :>> es => P.BoxSpecification -> Eff es E.BoxSpecification
evalBoxSpecification = \case
  P.Natural -> pure E.Natural
  P.To length -> E.To <$> Eval.evalLength length
  P.Spread length -> E.Spread <$> Eval.evalLength length

evalVariableAssignment :: [Error Eval.EvaluationError, EHexState] :>> es => P.VariableAssignment -> Eff es E.VariableAssignment
evalVariableAssignment = \case
  P.IntVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- Eval.evalQuantVariableAsVariable qVar
    eTgt <- Eval.evalInt tgt
    pure $ E.IntVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.LengthVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- Eval.evalQuantVariableAsVariable qVar
    eTgt <- Eval.evalLength tgt
    pure $ E.LengthVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.GlueVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- Eval.evalQuantVariableAsVariable qVar
    eTgt <- Eval.evalGlue tgt
    pure $ E.GlueVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.MathGlueVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- Eval.evalQuantVariableAsVariable qVar
    eTgt <- Eval.evalMathGlue tgt
    pure $ E.MathGlueVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.TokenListVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- Eval.evalQuantVariableAsVariable qVar
    eTgt <- Eval.evalTokenListAssignmentTarget tgt
    pure $ E.TokenListVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.SpecialIntParameterVariableAssignment specialIntParameter hexInt ->
    E.SpecialIntParameterVariableAssignment specialIntParameter <$> Eval.evalInt hexInt
  P.SpecialLengthParameterVariableAssignment specialLengthParameter length ->
    E.SpecialLengthParameterVariableAssignment specialLengthParameter <$> Eval.evalLength length

evalVariableModification :: [Error Eval.EvaluationError, EHexState] :>> es => P.VariableModification -> Eff es E.VariableModification
evalVariableModification = \case
  P.AdvanceIntVariable var arg -> do
    eVar <- Eval.evalQuantVariableAsVariable var
    eArg <- Eval.evalInt arg
    pure $ E.AdvanceIntVariable eVar eArg
  P.AdvanceLengthVariable var arg -> do
    eVar <- Eval.evalQuantVariableAsVariable var
    eArg <- Eval.evalLength arg
    pure $ E.AdvanceLengthVariable eVar eArg
  P.AdvanceGlueVariable var arg -> do
    eVar <- Eval.evalQuantVariableAsVariable var
    eArg <- Eval.evalGlue arg
    pure $ E.AdvanceGlueVariable eVar eArg
  P.AdvanceMathGlueVariable var arg -> do
    eVar <- Eval.evalQuantVariableAsVariable var
    eArg <- Eval.evalMathGlue arg
    pure $ E.AdvanceMathGlueVariable eVar eArg
  P.ScaleVariable scaleDirection numericVar arg -> do
    eVar <- evalNumericVariable numericVar
    eArg <- Eval.evalInt arg
    pure $ E.ScaleVariable scaleDirection eVar eArg

evalNumericVariable :: [Error Eval.EvaluationError, EHexState] :>> es => P.NumericVariable -> Eff es E.NumericVariable
evalNumericVariable = \case
  P.IntNumericVariable var -> E.IntNumericVariable <$> Eval.evalQuantVariableAsVariable var
  P.LengthNumericVariable var -> E.LengthNumericVariable <$> Eval.evalQuantVariableAsVariable var
  P.GlueNumericVariable var -> E.GlueNumericVariable <$> Eval.evalQuantVariableAsVariable var
  P.MathGlueNumericVariable var -> E.MathGlueNumericVariable <$> Eval.evalQuantVariableAsVariable var

evalControlSequenceTarget :: [Error Eval.EvaluationError, EHexState] :>> es => P.ControlSequenceTarget -> Eff es E.ControlSequenceTarget
evalControlSequenceTarget = \case
  P.MacroTarget macroDefinition ->
    pure $ E.MacroTarget macroDefinition
  P.LetTarget lexToken ->
    pure $ E.LetTarget lexToken
  P.FutureLetTarget futureLetTargetDefinition ->
    pure $ E.FutureLetTarget futureLetTargetDefinition
  P.ShortDefineTarget shortDefineTargetValue ->
    E.ShortDefineTarget <$> evalShortDefineTargetValue shortDefineTargetValue
  P.ReadTarget hexInt ->
    E.ReadTarget <$> Eval.evalInt hexInt
  P.FontTarget pFontFileSpec -> do
    eFontFileSpec <-
      E.FontFileSpec
        <$> evalFontSpecification pFontFileSpec.fontSpec
        <*> pure pFontFileSpec.fontPath
    pure $ E.FontTarget eFontFileSpec

evalShortDefineTargetValue :: [Error Eval.EvaluationError, EHexState] :>> es => P.ShortDefTargetValue -> Eff es PT.ShortDefTargetValue
evalShortDefineTargetValue (P.ShortDefTargetValue charryQuantityType tgtValInt) =
  PT.ShortDefTargetValue charryQuantityType <$> Eval.evalInt tgtValInt

evalFontSpecification :: [Error Eval.EvaluationError, EHexState] :>> es => P.FontSpecification -> Eff es TFM.FontSpecification
evalFontSpecification = \case
  P.NaturalFont -> pure TFM.NaturalFont
  P.FontAt len -> TFM.FontAt <$> Eval.evalLength len
  P.FontScaled n -> TFM.FontScaled <$> Eval.evalInt n

evalCodeAssignment :: [Error Eval.EvaluationError, EHexState] :>> es => P.CodeAssignment -> Eff es E.CodeAssignment
evalCodeAssignment codeAssignment = do
  -- Evaluate the index in the code-table, i.e. which char-code's property to set.
  codeIx <- Eval.evalCharCodeInt codeAssignment.codeTableRef.codeIndex
  -- Evaluate the value to set for the property, as an integer.
  vInt <- Eval.evalInt codeAssignment.codeValue
  -- Map the value to the appropriate type, given the code-type.
  codeValue <- case codeAssignment.codeTableRef.codeType of
    Code.CatCodeType ->
      E.CatCodeValue
        <$> Eval.noteRange @Code.CatCode vInt
    Code.MathCodeType -> do
      E.MathCodeValue
        <$> Eval.noteRange @Code.MathCode vInt
    Code.UpperCaseCodeType -> do
      E.UpperCaseCodeValue
        <$> Eval.noteRange @Code.UpperCaseCode vInt
    Code.LowerCaseCodeType -> do
      E.LowerCaseCodeValue
        <$> Eval.noteRange @Code.LowerCaseCode vInt
    Code.SpaceFactorCodeType -> do
      E.SpaceFactorCodeValue
        <$> Eval.noteRange @Code.SpaceFactorCode vInt
    Code.DelimiterCodeType -> do
      E.DelimiterCodeValue
        <$> Eval.noteRange @Code.DelimiterCode vInt
  pure $ E.CodeAssignment codeIx codeValue

evalMessageWriteCommand ::
  Error Eval.EvaluationError :> es =>
  P.MessageWriteCommand ->
  Eff es E.MessageWriteCommand
evalMessageWriteCommand cmd = do
  messageContents <- Eval.evalBalancedTextToText cmd.messageContents
  pure $ E.MessageWriteCommand {messageDest = cmd.messageDest, messageContents}
