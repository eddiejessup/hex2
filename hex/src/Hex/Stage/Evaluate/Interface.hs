module Hex.Stage.Evaluate.Interface where

import Hex.Stage.Evaluate.Interface.AST.Command qualified as Eval
import Hexlude
import qualified Hex.Stage.Parse.Interface.AST.Command as Uneval
import Hex.Stage.Parse.Interface (MonadCommandSource(..))

class MonadEvaluate m where
  evalCommand :: Uneval.Command -> m Eval.Command

getEvalCommand :: (MonadCommandSource m, MonadEvaluate m) => m (Maybe Eval.Command)
getEvalCommand = do
  getCommand >>= \case
    Nothing -> pure Nothing
    Just c -> do
      ec <- evalCommand c
      pure $ Just ec

-- A helper that's like `getEvalCommand`, but throws an error on end-of-input instead of returning `Nothing`.
getEvalCommandErrorEOF :: (MonadCommandSource m, MonadEvaluate m, MonadError e m) => e -> m Eval.Command
getEvalCommandErrorEOF = maybeToError getEvalCommand
