module Hex.Stage.Parse.Interface where

import Hex.Stage.Parse.Interface.AST.Command (Command)
import Hexlude

class Monad m => MonadCommandSource m where
  getCommand :: m (Maybe Command)

instance MonadCommandSource m => MonadCommandSource (StateT s m) where
  getCommand = lift getCommand
