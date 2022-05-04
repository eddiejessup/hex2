{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Evaluate.Impl where

import Hex.Stage.Evaluate.Interface.AST.Command qualified as E
import Hex.Stage.Evaluate.Impl.Command qualified as Eval
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hexlude

newtype MonadEvaluateT m a = MonadEvaluateT {unMonadEvaluateT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState st, MonadError e)

instance (MonadError e (MonadEvaluateT m), AsType Eval.EvaluationError e) => MonadEvaluate (MonadEvaluateT m) where
  evalCommand :: P.Command -> MonadEvaluateT m E.Command
  evalCommand = Eval.evalCommand
