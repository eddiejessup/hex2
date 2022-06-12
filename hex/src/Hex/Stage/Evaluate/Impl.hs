{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Evaluate.Impl where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Stage.Evaluate.Impl.Command qualified as Eval
import Hex.Stage.Evaluate.Impl.Common qualified as Eval
import Hex.Stage.Evaluate.Interface (MonadEvaluate (..))
import Hex.Stage.Evaluate.Interface.AST.Command qualified as E
import Hex.Stage.Parse.Interface.AST.Command qualified as P
import Hexlude

newtype MonadEvaluateT m a = MonadEvaluateT {unMonadEvaluateT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadError e,
      MonadHexState,
      MonadHexLog
    )

instance
  ( Monad m,
    MonadError e (MonadEvaluateT m),
    AsType Eval.EvaluationError e,
    MonadHexState (MonadEvaluateT m)
  ) =>
  MonadEvaluate (MonadEvaluateT m)
  where
  evalCommand :: P.Command -> MonadEvaluateT m E.Command
  evalCommand = Eval.evalCommand
