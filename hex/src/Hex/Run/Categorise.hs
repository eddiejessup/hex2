module Hex.Run.Categorise where

import Hex.Stage.Categorise.Types (RawCharCat (..), fmtRawCharCat)
import Hexlude
import Hex.Run.App
import Hex.Stage.Categorise.Interface (MonadCharCatSource(..))
import Hex.Common.HexState.Impl ()
import Hex.Stage.Categorise.Impl ()
import qualified Formatting as F

categoriseAll :: App [RawCharCat]
categoriseAll = go
  where
    go =
      getCharCat >>= \case
        Nothing ->
          pure []
        Just tok -> do
          v <- go
          pure $ tok : v

fmtCategoriseResult :: Fmt [RawCharCat] r
fmtCategoriseResult = F.unlined fmtRawCharCat
