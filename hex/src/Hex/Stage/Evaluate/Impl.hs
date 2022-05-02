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
    Uneval.VModeCommand Uneval.End -> pure $ Eval.VModeCommand Uneval.End
    _ -> pure $ Eval.ModeIndependentCommand Eval.Relax
