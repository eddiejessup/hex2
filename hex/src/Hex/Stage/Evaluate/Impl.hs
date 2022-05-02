{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Hex.Stage.Evaluate.Impl where

import Hexlude
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import qualified Hex.Stage.Parse.Interface.AST.Command as Uneval
import qualified Hex.Stage.Evaluate.Interface.AST.Command as Eval
import qualified Hex.Stage.Evaluate.Impl.Eval as Eval

instance Monad m => MonadEvaluate m where
  evalCommand :: Uneval.Command -> m Eval.Command
  evalCommand = \case
    Uneval.ShowToken lt -> pure $ Eval.ShowToken lt
    Uneval.ShowBox n -> Eval.ShowBox <$> Eval.evalInt n
    Uneval.VModeCommand vModeCmd -> Eval.VModeCommand <$> evalVModeCmd vModeCmd
    Uneval.ModeIndependentCommand modeIndepCmd -> Eval.ModeIndependentCommand <$> evalModeIndepCmd modeIndepCmd
    _ -> pure $ Eval.ModeIndependentCommand Eval.Relax

evalVModeCmd :: Monad m => Uneval.VModeCommand -> m Uneval.VModeCommand
evalVModeCmd = pure

evalModeIndepCmd :: Monad m => Uneval.ModeIndependentCommand -> m Eval.ModeIndependentCommand
evalModeIndepCmd = \case
  Uneval.Assign Uneval.Assignment {body, scope} -> do
    eBody <- evalAssignmentBody body
    pure $ Eval.Assign (Eval.Assignment eBody scope)
  Uneval.Relax -> panic "Not implemented"
  Uneval.IgnoreSpaces -> panic "Not implemented"
  Uneval.AddPenalty _hexInt -> panic "Not implemented"
  Uneval.AddKern _length -> panic "Not implemented"
  Uneval.AddMathKern _mathLength -> panic "Not implemented"
  Uneval.RemoveItem _removableItem -> panic "Not implemented"
  Uneval.SetAfterAssignmentToken _lexToken -> panic "Not implemented"
  Uneval.AddToAfterGroupTokens _lexToken -> panic "Not implemented"
  Uneval.WriteMessage _messageWriteCommand -> panic "Not implemented"
  Uneval.ModifyFileStream _fileStreamModificationCommand -> panic "Not implemented"
  Uneval.WriteToStream _streamWriteCommand -> panic "Not implemented"
  Uneval.DoSpecial _expandedBalancedText -> panic "Not implemented"
  Uneval.AddBox _boxPlacement _box -> panic "Not implemented"
  Uneval.ChangeScope _sign _commandTrigger -> panic "Not implemented"

evalAssignmentBody :: Uneval.AssignmentBody -> m Eval.AssignmentBody
evalAssignmentBody = \case

  Uneval.DefineControlSequence _controlSymbol _controlSequenceTarget -> panic "Not implemented"
  Uneval.SetVariable _variableAssignment -> panic "Not implemented"
  Uneval.ModifyVariable _variableModification -> panic "Not implemented"
  Uneval.AssignCode _codeAssignment -> panic "Not implemented"
  -- Uneval.AssignCode codeAssignment -> Eval.AssignCode <$> evalCodeAssignment codeAssignment
  Uneval.SelectFont _fontNumber -> panic "Not implemented"
  Uneval.SetFamilyMember _familyMember _fontRef -> panic "Not implemented"
  Uneval.SetParShape _hexInt _lengths -> panic "Not implemented"
  Uneval.SetBoxRegister _hexInt _box -> panic "Not implemented"
  Uneval.SetFontDimension _fontDimensionRef _length -> panic "Not implemented"
  Uneval.SetFontChar _fontCharRef _hexInt -> panic "Not implemented"
  Uneval.SetHyphenation _inhibitedBalancedText -> panic "Not implemented"
  Uneval.SetHyphenationPatterns _inhibitedBalancedText -> panic "Not implemented"
  Uneval.SetBoxDimension _boxDimensionRef _length -> panic "Not implemented"
  Uneval.SetInteractionMode _interactionMode -> panic "Not implemented"

evalCodeAssignment :: Monad m => Uneval.CodeAssignment -> m Uneval.CodeAssignment
evalCodeAssignment = pure
