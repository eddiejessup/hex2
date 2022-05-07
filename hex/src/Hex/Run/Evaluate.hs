module Hex.Run.Evaluate where

import Formatting qualified as F
import Hex.Run.App (App)
import Hex.Stage.Evaluate.Interface (getEvalCommand)
import Hex.Stage.Evaluate.Interface.AST.Command (Command)
import Hexlude

evaluateAll :: App [Command]
evaluateAll = go
  where
    go =
      getEvalCommand >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtEvalCommandList :: Fmt [Command]
fmtEvalCommandList = F.unlined fmtOneCommand
  where
    fmtOneCommand =
      F.shown
