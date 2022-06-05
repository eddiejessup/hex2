module Hex.Stage.Evaluate.Impl.Command where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.Quantity qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as E
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as E
import Hex.Stage.Interpret.Build.Box.Elem qualified as Box
import Hex.Stage.Interpret.Build.Box.Elem qualified as Elem
import Hex.Stage.Interpret.Build.List.Elem qualified as Elem
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hex.Stage.Parse.Interface.AST.Quantity qualified as P
import Hexlude

evalCommand :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.Command -> m E.Command
evalCommand = \case
  P.ShowToken lt -> pure $ E.ShowToken lt
  P.ShowBox n -> E.ShowBox <$> Eval.evalInt n
  P.VModeCommand vModeCmd -> E.VModeCommand <$> evalVModeCmd vModeCmd
  P.ModeIndependentCommand modeIndepCmd -> E.ModeIndependentCommand <$> evalModeIndepCmd modeIndepCmd
  _ -> pure $ E.ModeIndependentCommand E.Relax

evalVModeCmd :: Monad m => P.VModeCommand -> m P.VModeCommand
evalVModeCmd = pure

evalModeIndepCmd :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.ModeIndependentCommand -> m E.ModeIndependentCommand
evalModeIndepCmd = \case
  P.Assign P.Assignment {body, scope} -> do
    eBody <- evalAssignmentBody body
    pure $ E.Assign (E.Assignment eBody scope)
  P.Relax -> pure E.Relax
  P.IgnoreSpaces -> pure E.IgnoreSpaces
  P.AddPenalty hexInt -> E.AddPenalty . Elem.Penalty <$> Eval.evalInt hexInt
  P.AddKern length -> E.AddKern . Elem.Kern <$> Eval.evalLength length
  P.AddMathKern mathLength -> E.AddMathKern <$> Eval.evalMathLength mathLength
  P.RemoveItem _removableItem -> notImplemented "evalModeIndepCmd 'RemoveItem'"
  P.SetAfterAssignmentToken lexToken -> pure $ E.SetAfterAssignmentToken lexToken
  P.AddToAfterGroupTokens lexToken -> pure $ E.AddToAfterGroupTokens lexToken
  P.WriteMessage messageWriteCommand -> E.WriteMessage <$> evalMessageWriteCommand messageWriteCommand
  P.ModifyFileStream _fileStreamModificationCommand -> notImplemented "evalModeIndepCmd 'ModifyFileStream'"
  P.WriteToStream streamWriteCommand -> E.WriteToStream <$> evalStreamWriteCommand streamWriteCommand
  P.DoSpecial expandedBalancedText -> pure $ E.DoSpecial expandedBalancedText
  P.AddBox boxPlacement box -> pure $ E.AddBox boxPlacement box
  P.ChangeScope sign localStructureTrigger -> pure $ E.ChangeScope sign localStructureTrigger
  P.DebugShowState -> pure E.DebugShowState

evalStreamWriteCommand :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.StreamWriteCommand -> m E.StreamWriteCommand
evalStreamWriteCommand (P.StreamWriteCommand n writeText) =
  E.StreamWriteCommand <$> Eval.evalInt n <*> evalWriteText writeText

evalWriteText :: (MonadError e m, AsType Eval.EvaluationError e) => P.WriteText -> m E.WriteText
evalWriteText = \case
  P.ImmediateWriteText expandedBalancedText ->
    E.ImmediateWriteText <$> Eval.evalExpandedBalancedTextToText expandedBalancedText
  P.DeferredWriteText inhibitedBalancedText ->
    pure $ E.DeferredWriteText inhibitedBalancedText

evalAssignmentBody :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.AssignmentBody -> m E.AssignmentBody
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
  P.SetHyphenation inhibitedBalancedText -> pure $ E.SetHyphenation inhibitedBalancedText
  P.SetHyphenationPatterns inhibitedBalancedText -> pure $ E.SetHyphenationPatterns inhibitedBalancedText
  P.SetBoxDimension boxDimensionRef length -> pure $ E.SetBoxDimension boxDimensionRef length
  P.SetInteractionMode interactionMode -> pure $ E.SetInteractionMode interactionMode

evalFontSpecialCharRef :: HSt.MonadHexState m => P.FontSpecialCharRef -> m E.FontSpecialCharRef
evalFontSpecialCharRef (P.FontSpecialCharRef fontSpecialChar fontNr) = E.FontSpecialCharRef fontSpecialChar <$> Eval.evalFontRef fontNr

evalBox :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.Box -> m E.Box
evalBox = \case
  P.FetchedRegisterBox boxFetchMode loc -> E.FetchedRegisterBox boxFetchMode <$> Eval.evalExplicitRegisterLocation loc
  P.LastBox -> pure E.LastBox
  P.VSplitBox n length -> E.VSplitBox <$> Eval.evalInt n <*> Eval.evalLength length
  P.ExplicitBox boxSpecification explicitBoxType -> E.ExplicitBox <$> evalBoxSpecification boxSpecification <*> pure explicitBoxType

evalBoxSpecification :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.BoxSpecification -> m E.BoxSpecification
evalBoxSpecification = \case
  P.Natural -> pure E.Natural
  P.To length -> E.To <$> Eval.evalLength length
  P.Spread length -> E.Spread <$> Eval.evalLength length

evalVariableAssignment :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.VariableAssignment -> m E.VariableAssignment
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

evalVariableModification :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.VariableModification -> m E.VariableModification
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

evalNumericVariable :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.NumericVariable -> m E.NumericVariable
evalNumericVariable = \case
  P.IntNumericVariable var -> E.IntNumericVariable <$> Eval.evalQuantVariableAsVariable var
  P.LengthNumericVariable var -> E.LengthNumericVariable <$> Eval.evalQuantVariableAsVariable var
  P.GlueNumericVariable var -> E.GlueNumericVariable <$> Eval.evalQuantVariableAsVariable var
  P.MathGlueNumericVariable var -> E.MathGlueNumericVariable <$> Eval.evalQuantVariableAsVariable var

evalControlSequenceTarget :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.ControlSequenceTarget -> m E.ControlSequenceTarget
evalControlSequenceTarget = \case
  P.MacroTarget macroDefinition ->
    pure $ E.MacroTarget macroDefinition
  P.LetTarget lexToken ->
    pure $ E.LetTarget lexToken
  P.FutureLetTarget futureLetTargetDefinition ->
    pure $ E.FutureLetTarget futureLetTargetDefinition
  P.ShortDefineTarget charryQuantityType tgtValInt ->
    E.ShortDefineTarget charryQuantityType <$> Eval.evalInt tgtValInt
  P.ReadTarget hexInt ->
    E.ReadTarget <$> Eval.evalInt hexInt
  P.FontTarget pFontFileSpec -> do
    eFontFileSpec <-
      E.FontFileSpec
        <$> evalFontSpecification pFontFileSpec.fontSpec
        <*> pure pFontFileSpec.fontPath
    pure $ E.FontTarget eFontFileSpec

evalFontSpecification :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.FontSpecification -> m Box.FontSpecification
evalFontSpecification = \case
  P.NaturalFont -> pure Box.NaturalFont
  P.FontAt len -> Box.FontAt <$> Eval.evalLength len
  P.FontScaled n -> Box.FontScaled <$> Eval.evalInt n

evalCodeAssignment :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.CodeAssignment -> m E.CodeAssignment
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
  (MonadError e m, AsType Eval.EvaluationError e) =>
  P.MessageWriteCommand ->
  m E.MessageWriteCommand
evalMessageWriteCommand cmd = do
  messageContents <- Eval.evalExpandedBalancedTextToText cmd.messageContents
  pure $ E.MessageWriteCommand {messageDest = cmd.messageDest, messageContents}
