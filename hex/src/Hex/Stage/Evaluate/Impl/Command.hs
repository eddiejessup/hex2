module Hex.Stage.Evaluate.Impl.Command where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Impl.Scoped.Scope (RegisterLocation (..))
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.Quantity qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as E
import Hex.Stage.Interpret.Build.Box.Elem qualified as Box
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
  P.Relax -> notImplemented "evalModeIndepCmd 'Relax'"
  P.IgnoreSpaces -> notImplemented "evalModeIndepCmd 'IgnoreSpaces'"
  P.AddPenalty _hexInt -> notImplemented "evalModeIndepCmd 'AddPenalty'"
  P.AddKern _length -> notImplemented "evalModeIndepCmd 'AddKern'"
  P.AddMathKern _mathLength -> notImplemented "evalModeIndepCmd 'AddMathKern'"
  P.RemoveItem _removableItem -> notImplemented "evalModeIndepCmd 'RemoveItem'"
  P.SetAfterAssignmentToken _lexToken -> notImplemented "evalModeIndepCmd 'SetAfterAssignmentToken'"
  P.AddToAfterGroupTokens _lexToken -> notImplemented "evalModeIndepCmd 'AddToAfterGroupTokens'"
  P.WriteMessage messageWriteCommand -> E.WriteMessage <$> evalMessageWriteCommand messageWriteCommand
  P.ModifyFileStream _fileStreamModificationCommand -> notImplemented "evalModeIndepCmd 'ModifyFileStream'"
  P.WriteToStream _streamWriteCommand -> notImplemented "evalModeIndepCmd 'WriteToStream'"
  P.DoSpecial _expandedBalancedText -> notImplemented "evalModeIndepCmd 'DoSpecial'"
  P.AddBox _boxPlacement _box -> notImplemented "evalModeIndepCmd 'AddBox'"
  P.ChangeScope _sign _commandTrigger -> notImplemented "evalModeIndepCmd 'ChangeScope'"
  P.DebugShowState -> pure E.DebugShowState

evalAssignmentBody :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.AssignmentBody -> m E.AssignmentBody
evalAssignmentBody = \case
  P.DefineControlSequence controlSymbol controlSequenceTarget ->
    E.DefineControlSequence controlSymbol <$> evalControlSequenceTarget controlSequenceTarget
  P.SetVariable variableAssignment ->
    E.SetVariable <$> evalVariableAssignment variableAssignment
  P.ModifyVariable _variableModification -> notImplemented "evalAssignmentBody 'ModifyVariable'"
  P.AssignCode codeAssignment -> E.AssignCode <$> evalCodeAssignment codeAssignment
  P.SelectFont _fontNumber -> notImplemented "evalAssignmentBody 'SelectFont'"
  P.SetFamilyMember _familyMember _fontRef -> notImplemented "evalAssignmentBody 'SetFamilyMember'"
  P.SetParShape _hexInt _lengths -> notImplemented "evalAssignmentBody 'SetParShape'"
  P.SetBoxRegister _hexInt _box -> notImplemented "evalAssignmentBody 'SetBoxRegister'"
  P.SetFontDimension _fontDimensionRef _length -> notImplemented "evalAssignmentBody 'SetFontDimension'"
  P.SetFontChar _fontCharRef _hexInt -> notImplemented "evalAssignmentBody 'SetFontChar'"
  P.SetHyphenation _inhibitedBalancedText -> notImplemented "evalAssignmentBody 'SetHyphenation'"
  P.SetHyphenationPatterns _inhibitedBalancedText -> notImplemented "evalAssignmentBody 'SetHyphenationPatterns'"
  P.SetBoxDimension _boxDimensionRef _length -> notImplemented "evalAssignmentBody 'SetBoxDimension'"
  P.SetInteractionMode _interactionMode -> notImplemented "evalAssignmentBody 'SetInteractionMode'"

evalVariableAssignment :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.VariableAssignment -> m E.VariableAssignment
evalVariableAssignment = \case
  P.IntVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- evalQuantVariable qVar
    eTgt <- Eval.evalInt tgt
    pure $ E.IntVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.LengthVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- evalQuantVariable qVar
    eTgt <- Eval.evalLength tgt
    pure $ E.LengthVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.GlueVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- evalQuantVariable qVar
    eTgt <- Eval.evalGlue tgt
    pure $ E.GlueVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.MathGlueVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- evalQuantVariable qVar
    eTgt <- Eval.evalMathGlue tgt
    pure $ E.MathGlueVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.TokenListVariableAssignment (P.QuantVariableAssignment qVar tgt) -> do
    quantVariableEval <- evalQuantVariable qVar
    eTgt <- Eval.evalTokenListAssignmentTarget tgt
    pure $ E.TokenListVariableAssignment $ E.QuantVariableAssignment quantVariableEval eTgt
  P.SpecialIntParameterVariableAssignment specialIntParameter hexInt ->
    E.SpecialIntParameterVariableAssignment specialIntParameter <$> Eval.evalInt hexInt
  P.SpecialLengthParameterVariableAssignment specialLengthParameter length ->
    E.SpecialLengthParameterVariableAssignment specialLengthParameter <$> Eval.evalLength length

evalQuantVariable :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.QuantVariableAST a -> m (E.QuantVariableEval a)
evalQuantVariable = \case
  P.ParamVar intParam -> pure $ E.ParamVar intParam
  P.RegisterVar registerLocation -> E.RegisterVar <$> evalRegisterLocation registerLocation

evalRegisterLocation :: (MonadError e m, AsType Eval.EvaluationError e, HSt.MonadHexState m) => P.RegisterLocation -> m RegisterLocation
evalRegisterLocation = \case
  P.ExplicitRegisterLocation hexInt -> RegisterLocation <$> Eval.evalInt hexInt
  P.InternalRegisterLocation evalInt -> pure $ RegisterLocation evalInt

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
    PT.CategoryCodeType ->
      E.CatCodeValue
        <$> Eval.noteRange @Code.CatCode vInt
    PT.MathCodeType -> do
      E.MathCodeValue
        <$> Eval.noteRange @Code.MathCode vInt
    PT.UpperCaseCodeType -> do
      E.UpperCaseCodeValue
        <$> Eval.noteRange @Code.UpperCaseCode vInt
    PT.LowerCaseCodeType -> do
      E.LowerCaseCodeValue
        <$> Eval.noteRange @Code.LowerCaseCode vInt
    PT.SpaceFactorCodeType -> do
      E.SpaceFactorCodeValue
        <$> Eval.noteRange @Code.SpaceFactorCode vInt
    PT.DelimiterCodeType -> do
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
