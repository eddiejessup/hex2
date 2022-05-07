module Hex.Run.Categorise where

import Formatting qualified as F
import Hex.Run.App
import Hex.Stage.Categorise.Interface (MonadCharCatSource (..), RawCharCat (..), fmtRawCharCat)
import Hexlude

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

fmtCategoriseResult :: Fmt [RawCharCat]
fmtCategoriseResult = F.unlined fmtRawCharCat
