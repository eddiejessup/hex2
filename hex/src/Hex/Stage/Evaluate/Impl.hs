{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Evaluate.Impl where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Stage.Evaluate.Impl.Eval qualified as Eval
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Evaluate.Interface.AST.Common qualified as Eval
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hex.Stage.Parse.Interface.AST.Common qualified as Uneval
import Hexlude

newtype MonadEvaluateT m a = MonadEvaluateT {unMonadEvaluateT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState st, MonadError e)

instance (MonadError e (MonadEvaluateT m), AsType Eval.EvaluationError e) => MonadEvaluate (MonadEvaluateT m) where
  evalCommand :: Uneval.Command -> MonadEvaluateT m Eval.Command
  evalCommand = \case
    Uneval.ShowToken lt -> pure $ Eval.ShowToken lt
    Uneval.ShowBox n -> Eval.ShowBox <$> Eval.evalInt n
    Uneval.VModeCommand vModeCmd -> Eval.VModeCommand <$> evalVModeCmd vModeCmd
    Uneval.ModeIndependentCommand modeIndepCmd -> Eval.ModeIndependentCommand <$> evalModeIndepCmd modeIndepCmd
    _ -> pure $ Eval.ModeIndependentCommand Eval.Relax

evalVModeCmd :: Monad m => Uneval.VModeCommand -> m Uneval.VModeCommand
evalVModeCmd = pure

evalModeIndepCmd :: (MonadError e m, AsType Eval.EvaluationError e) => Uneval.ModeIndependentCommand -> m Eval.ModeIndependentCommand
evalModeIndepCmd = \case
  Uneval.Assign Uneval.Assignment {body, scope} -> do
    eBody <- evalAssignmentBody body
    pure $ Eval.Assign (Eval.Assignment eBody scope)
  Uneval.Relax -> notImplemented "evalModeIndepCmd 'Relax'"
  Uneval.IgnoreSpaces -> notImplemented "evalModeIndepCmd 'IgnoreSpaces'"
  Uneval.AddPenalty _hexInt -> notImplemented "evalModeIndepCmd 'AddPenalty'"
  Uneval.AddKern _length -> notImplemented "evalModeIndepCmd 'AddKern'"
  Uneval.AddMathKern _mathLength -> notImplemented "evalModeIndepCmd 'AddMathKern'"
  Uneval.RemoveItem _removableItem -> notImplemented "evalModeIndepCmd 'RemoveItem'"
  Uneval.SetAfterAssignmentToken _lexToken -> notImplemented "evalModeIndepCmd 'SetAfterAssignmentToken'"
  Uneval.AddToAfterGroupTokens _lexToken -> notImplemented "evalModeIndepCmd 'AddToAfterGroupTokens'"
  Uneval.WriteMessage _messageWriteCommand -> notImplemented "evalModeIndepCmd 'WriteMessage'"
  Uneval.ModifyFileStream _fileStreamModificationCommand -> notImplemented "evalModeIndepCmd 'ModifyFileStream'"
  Uneval.WriteToStream _streamWriteCommand -> notImplemented "evalModeIndepCmd 'WriteToStream'"
  Uneval.DoSpecial _expandedBalancedText -> notImplemented "evalModeIndepCmd 'DoSpecial'"
  Uneval.AddBox _boxPlacement _box -> notImplemented "evalModeIndepCmd 'AddBox'"
  Uneval.ChangeScope _sign _commandTrigger -> notImplemented "evalModeIndepCmd 'ChangeScope'"

evalAssignmentBody :: (MonadError e m, AsType Eval.EvaluationError e) => Uneval.AssignmentBody -> m Eval.AssignmentBody
evalAssignmentBody = \case
  Uneval.DefineControlSequence _controlSymbol _controlSequenceTarget -> notImplemented "evaluate 'DefineControlSequence'"
  Uneval.SetVariable _variableAssignment -> notImplemented "evalAssignmentBody 'SetVariable'"
  Uneval.ModifyVariable _variableModification -> notImplemented "evalAssignmentBody 'ModifyVariable'"
  Uneval.AssignCode codeAssignment -> Eval.AssignCode <$> evalCodeAssignment codeAssignment
  Uneval.SelectFont _fontNumber -> notImplemented "evalAssignmentBody 'SelectFont'"
  Uneval.SetFamilyMember _familyMember _fontRef -> notImplemented "evalAssignmentBody 'SetFamilyMember'"
  Uneval.SetParShape _hexInt _lengths -> notImplemented "evalAssignmentBody 'SetParShape'"
  Uneval.SetBoxRegister _hexInt _box -> notImplemented "evalAssignmentBody 'SetBoxRegister'"
  Uneval.SetFontDimension _fontDimensionRef _length -> notImplemented "evalAssignmentBody 'SetFontDimension'"
  Uneval.SetFontChar _fontCharRef _hexInt -> notImplemented "evalAssignmentBody 'SetFontChar'"
  Uneval.SetHyphenation _inhibitedBalancedText -> notImplemented "evalAssignmentBody 'SetHyphenation'"
  Uneval.SetHyphenationPatterns _inhibitedBalancedText -> notImplemented "evalAssignmentBody 'SetHyphenationPatterns'"
  Uneval.SetBoxDimension _boxDimensionRef _length -> notImplemented "evalAssignmentBody 'SetBoxDimension'"
  Uneval.SetInteractionMode _interactionMode -> notImplemented "evalAssignmentBody 'SetInteractionMode'"

evalCodeAssignment :: (MonadError e m, AsType Eval.EvaluationError e) => Uneval.CodeAssignment -> m Eval.CodeAssignment
evalCodeAssignment codeAssignment = do
  -- Evaluate the index in the code-table, i.e. which char-code's property to set.
  codeIx <- Eval.evalCharCodeInt codeAssignment.codeTableRef.codeIndex
  -- Evaluate the value to set for the property, as an integer.
  vInt <- Eval.evalInt codeAssignment.codeValue
  -- Map the value to the appropriate type, given the code-type.
  codeValue <- case codeAssignment.codeTableRef.codeType of
    PT.CategoryCodeType ->
      Eval.CatCodeValue
        <$> Eval.noteRange @Code.CatCode vInt
    PT.MathCodeType -> do
      Eval.MathCodeValue
        <$> Eval.noteRange @Code.MathCode vInt
    PT.ChangeCaseCodeType letterCase -> do
      Eval.ChangeCaseCodeValue letterCase
        <$> Eval.noteRange @Code.CaseChangeCode vInt
    PT.SpaceFactorCodeType -> do
      Eval.SpaceFactorCodeValue
        <$> Eval.noteRange @Code.SpaceFactorCode vInt
    PT.DelimiterCodeType -> do
      Eval.DelimiterCodeValue
        <$> Eval.noteRange @Code.DelimiterCode vInt
  pure $ Eval.CodeAssignment codeIx codeValue

evalCodeTableRef :: (MonadError e m, AsType Eval.EvaluationError e) => Uneval.CodeTableRef -> m Eval.CodeTableRef
evalCodeTableRef codeTableRef =
  Eval.CodeTableRef
    <$> pure codeTableRef.codeType
    <*> Eval.evalCharCodeInt codeTableRef.codeIndex
