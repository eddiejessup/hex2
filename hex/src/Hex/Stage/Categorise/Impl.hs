{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Categorise.Impl where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString qualified as BS
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as H.St
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Categorise.Interface (MonadCharCatSource (..))
import Hex.Stage.Categorise.Types
import Hexlude

extractCharCat ::
  HSt.MonadHexState m => ByteString -> m (Maybe (RawCharCat, ByteString))
extractCharCat xs = runMaybeT $ do
  (n1, rest1) <- MaybeT $ pure $ BS.uncons xs
  -- Next two characters must be identical, and have category
  -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
  cat1 <- lift $ H.St.getCategory (Code.CharCode n1)
  let normal = (RawCharCat (Code.CharCode n1) cat1, rest1)
  case cat1 of
    Code.CoreCatCode Code.Superscript -> case BS.uncons rest1 of
      Just (n2, rest2) | n1 == n2 ->
        case BS.uncons rest2 of
          Just (n3, rest3) ->
            lift (H.St.getCategory (Code.CharCode n3)) >>= \case
              Code.EndOfLine ->
                pure normal
              _ -> do
                let char3Triod = Code.CharCode $ if n3 < 64 then n3 + 64 else n3 - 64
                cat3 <- lift $ H.St.getCategory char3Triod
                pure (RawCharCat char3Triod cat3, rest3)
          Nothing ->
            pure normal
      _ ->
        pure normal
    _ ->
      pure normal

instance
  ( Monad m,
    MonadState st m,
    HasType ByteString st,
    HSt.MonadHexState m
  ) =>
  MonadCharCatSource m
  where
  -- Lift 'extractCharCat' into a stateful context.
  getCharCat = do
    bs <- use (typed @ByteString)
    extractCharCat bs >>= \case
      Nothing -> pure Nothing
      Just (c, bs') -> do
        assign' (typed @ByteString) bs'
        pure $ Just c