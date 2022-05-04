module Hex.Stage.Evaluate.Impl.Command where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Impl.Quantity qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Command qualified as E
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as E
import Hex.Stage.Interpret.Build.Box.Elem qualified as Box
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hex.Stage.Parse.Interface.AST.Quantity qualified as P
import Hexlude

evalCommand :: (MonadError e m, AsType Eval.EvaluationError e) => P.Command -> m E.Command
evalCommand = \case
  P.ShowToken lt -> pure $ E.ShowToken lt
  P.ShowBox n -> E.ShowBox <$> Eval.evalInt n
  P.VModeCommand vModeCmd -> E.VModeCommand <$> evalVModeCmd vModeCmd
  P.ModeIndependentCommand modeIndepCmd -> E.ModeIndependentCommand <$> evalModeIndepCmd modeIndepCmd
  _ -> pure $ E.ModeIndependentCommand E.Relax

evalVModeCmd :: Monad m => P.VModeCommand -> m P.VModeCommand
evalVModeCmd = pure

evalModeIndepCmd :: (MonadError e m, AsType Eval.EvaluationError e) => P.ModeIndependentCommand -> m E.ModeIndependentCommand
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
  P.WriteMessage _messageWriteCommand -> notImplemented "evalModeIndepCmd 'WriteMessage'"
  P.ModifyFileStream _fileStreamModificationCommand -> notImplemented "evalModeIndepCmd 'ModifyFileStream'"
  P.WriteToStream _streamWriteCommand -> notImplemented "evalModeIndepCmd 'WriteToStream'"
  P.DoSpecial _expandedBalancedText -> notImplemented "evalModeIndepCmd 'DoSpecial'"
  P.AddBox _boxPlacement _box -> notImplemented "evalModeIndepCmd 'AddBox'"
  P.ChangeScope _sign _commandTrigger -> notImplemented "evalModeIndepCmd 'ChangeScope'"

evalAssignmentBody :: (MonadError e m, AsType Eval.EvaluationError e) => P.AssignmentBody -> m E.AssignmentBody
evalAssignmentBody = \case
  P.DefineControlSequence controlSymbol controlSequenceTarget ->
    E.DefineControlSequence controlSymbol <$> evalControlSequenceTarget controlSequenceTarget
  P.SetVariable _variableAssignment -> notImplemented "evalAssignmentBody 'SetVariable'"
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

evalControlSequenceTarget :: (MonadError e m) => P.ControlSequenceTarget -> m E.ControlSequenceTarget
evalControlSequenceTarget = \case
  P.MacroTarget macroDefinition -> do
    pure $ E.NonFontTarget $ Res.SyntaxCommandHeadToken $ ST.MacroTok macroDefinition
  P.LetTarget _lexToken -> do
    notImplemented "evalControlSequenceTarget 'LetTarget'"
  P.FutureLetTarget _futureLetTargetDefinition ->
    notImplemented "evalControlSequenceTarget 'FutureLetTarget'"
  P.ShortDefineTarget _charryQuantityType _n -> do
    notImplemented "evalControlSequenceTarget 'ShortDefineTarget'"
  P.ReadTarget _hexInt -> do
    notImplemented "evalControlSequenceTarget 'ReadTarget'"
  P.FontTarget pFontFileSpec -> do
    eFontFileSpec <-
      E.FontFileSpec
        <$> evalFontSpecification pFontFileSpec.fontSpec
        <*> pure pFontFileSpec.fontPath
    pure $ E.FontTarget eFontFileSpec

evalFontSpecification :: (MonadError e m) => P.FontSpecification -> m Box.FontSpecification
evalFontSpecification = \case
  P.NaturalFont -> pure Box.NaturalFont
  P.FontAt len -> Box.FontAt <$> Eval.evalLength len
  P.FontScaled n -> Box.FontScaled <$> Eval.evalInt n

evalCodeAssignment :: (MonadError e m, AsType Eval.EvaluationError e) => P.CodeAssignment -> m E.CodeAssignment
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
    PT.ChangeCaseCodeType letterCase -> do
      E.ChangeCaseCodeValue letterCase
        <$> Eval.noteRange @Code.CaseChangeCode vInt
    PT.SpaceFactorCodeType -> do
      E.SpaceFactorCodeValue
        <$> Eval.noteRange @Code.SpaceFactorCode vInt
    PT.DelimiterCodeType -> do
      E.DelimiterCodeValue
        <$> Eval.noteRange @Code.DelimiterCode vInt
  pure $ E.CodeAssignment codeIx codeValue

evalCodeTableRef :: (MonadError e m, AsType Eval.EvaluationError e) => P.CodeTableRef -> m E.CodeTableRef
evalCodeTableRef codeTableRef =
  E.CodeTableRef
    <$> pure codeTableRef.codeType
    <*> Eval.evalCharCodeInt codeTableRef.codeIndex
