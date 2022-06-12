module Hex.Stage.Evaluate.Interface where

import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hex.Stage.Parse.Interface (MonadCommandSource (..))
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hexlude

class Monad m => MonadEvaluate m where
  evalCommand :: Uneval.Command -> m Eval.Command

getEvalCommand :: (MonadCommandSource m, MonadEvaluate m) => m (Maybe Eval.Command)
getEvalCommand =
  getCommand >>= \case
    Nothing -> pure Nothing
    Just c -> Just <$> evalCommand c

-- A helper that's like `getEvalCommand`, but throws an error on end-of-input instead of returning `Nothing`.
getEvalCommandErrorEOF :: (MonadCommandSource m, MonadEvaluate m, MonadError e m) => e -> m Eval.Command
getEvalCommandErrorEOF = nothingToError getEvalCommand

instance MonadEvaluate m => MonadEvaluate (StateT s m) where
  evalCommand x = lift $ evalCommand x
