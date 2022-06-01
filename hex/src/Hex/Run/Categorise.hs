module Hex.Run.Categorise where

import Formatting qualified as F
import Hex.Stage.Categorise.Interface (MonadCharCatSource (..), RawCharCat (..), fmtRawCharCat)
import Hexlude

categoriseAll :: MonadCharCatSource m => m [RawCharCat]
categoriseAll = go
  where
    go =
      extractCharCat >>= \case
        Nothing ->
          pure []
        Just tok -> do
          v <- go
          pure $ tok : v

fmtCategoriseResult :: Fmt [RawCharCat]
fmtCategoriseResult = F.unlined fmtRawCharCat
