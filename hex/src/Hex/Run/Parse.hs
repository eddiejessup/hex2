module Hex.Run.Parse where

import Formatting qualified as F
import Hex.Stage.Parse.Interface (CommandSource (..), getCommand)
import Hex.Stage.Parse.Interface.AST.Command (Command)
import Hexlude

parseAll :: CommandSource :> es => Eff es [Command]
parseAll = go
  where
    go =
      getCommand >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtCommandList :: Fmt [Command]
fmtCommandList = F.unlined fmtOneCommand
  where
    fmtOneCommand =
      F.shown
