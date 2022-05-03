{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Evaluate.Impl where

import Hex.Stage.Evaluate.Impl.Eval qualified as Eval
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hexlude
import qualified Hex.Stage.Evaluate.Interface.AST.Common as Eval
import qualified Hex.Stage.Parse.Interface.AST.Common as Uneval

instance (MonadError e m, AsType Eval.EvaluationError e) => MonadEvaluate m where
  evalCommand :: Uneval.Command -> m Eval.Command
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
  Uneval.Relax -> panic "Not implemented: Relax "
  Uneval.IgnoreSpaces -> panic "Not implemented: IgnoreSpaces "
  Uneval.AddPenalty _hexInt -> panic "Not implemented: AddPenalty "
  Uneval.AddKern _length -> panic "Not implemented: AddKern "
  Uneval.AddMathKern _mathLength -> panic "Not implemented: AddMathKern "
  Uneval.RemoveItem _removableItem -> panic "Not implemented: RemoveItem "
  Uneval.SetAfterAssignmentToken _lexToken -> panic "Not implemented: SetAfterAssignmentToken "
  Uneval.AddToAfterGroupTokens _lexToken -> panic "Not implemented: AddToAfterGroupTokens "
  Uneval.WriteMessage _messageWriteCommand -> panic "Not implemented: WriteMessage "
  Uneval.ModifyFileStream _fileStreamModificationCommand -> panic "Not implemented: ModifyFileStream "
  Uneval.WriteToStream _streamWriteCommand -> panic "Not implemented: WriteToStream "
  Uneval.DoSpecial _expandedBalancedText -> panic "Not implemented: DoSpecial "
  Uneval.AddBox _boxPlacement _box -> panic "Not implemented: AddBox "
  Uneval.ChangeScope _sign _commandTrigger -> panic "Not implemented: ChangeScope "

evalAssignmentBody :: (MonadError e m, AsType Eval.EvaluationError e) => Uneval.AssignmentBody -> m Eval.AssignmentBody
evalAssignmentBody = \case
  Uneval.DefineControlSequence _controlSymbol _controlSequenceTarget -> panic "Not implemented: DefineControlSequence "
  Uneval.SetVariable _variableAssignment -> panic "Not implemented: SetVariable "
  Uneval.ModifyVariable _variableModification -> panic "Not implemented: ModifyVariable "
  Uneval.AssignCode codeAssignment -> Eval.AssignCode <$> evalCodeAssignment codeAssignment
  Uneval.SelectFont _fontNumber -> panic "Not implemented: SelectFont "
  Uneval.SetFamilyMember _familyMember _fontRef -> panic "Not implemented: SetFamilyMember "
  Uneval.SetParShape _hexInt _lengths -> panic "Not implemented: SetParShape "
  Uneval.SetBoxRegister _hexInt _box -> panic "Not implemented: SetBoxRegister "
  Uneval.SetFontDimension _fontDimensionRef _length -> panic "Not implemented: SetFontDimension "
  Uneval.SetFontChar _fontCharRef _hexInt -> panic "Not implemented: SetFontChar "
  Uneval.SetHyphenation _inhibitedBalancedText -> panic "Not implemented: SetHyphenation "
  Uneval.SetHyphenationPatterns _inhibitedBalancedText -> panic "Not implemented: SetHyphenationPatterns "
  Uneval.SetBoxDimension _boxDimensionRef _length -> panic "Not implemented: SetBoxDimension "
  Uneval.SetInteractionMode _interactionMode -> panic "Not implemented: SetInteractionMode "

evalCodeAssignment :: (MonadError e m, AsType Eval.EvaluationError e) => Uneval.CodeAssignment -> m Eval.CodeAssignment
evalCodeAssignment codeAssignment =
  Eval.CodeAssignment
  <$> evalCodeTableRef codeAssignment.codeTableRef
  <*> Eval.evalInt codeAssignment.codeValue

evalCodeTableRef :: (MonadError e m, AsType Eval.EvaluationError e) => Uneval.CodeTableRef -> m Eval.CodeTableRef
evalCodeTableRef codeTableRef =
  Eval.CodeTableRef
  <$> pure codeTableRef.codeType
  <*> Eval.evalCharCodeInt codeTableRef.codeIndex
