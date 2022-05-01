module Hex.Run.Parse where

import Hexlude
import Hex.Run.App (App)
import Hex.Common.HexState.Impl ()
import Hex.Stage.Categorise.Impl ()
import Hex.Stage.Lex.Impl ()
import Hex.Stage.Resolve.Impl ()
import Hex.Stage.Expand.Impl ()
import Hex.Stage.Parse.Impl ()
import qualified Formatting as F
import Hex.Stage.Parse.Interface.AST.Command (Command)
import Hex.Stage.Parse.Interface (MonadCommandSource(..))

parseAll :: App [Command]
parseAll = go
  where
    go =
      getCommand >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtCommandList :: Fmt [Command] r
fmtCommandList = F.unlined fmtOneCommand
  where
    fmtOneCommand =
      F.shown
