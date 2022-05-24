{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Categorise.Impl where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString qualified as BS
import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Categorise.Interface (MonadCharCatSource (..), RawCharCat (..), fmtRawCharCat)
import Hexlude

extractCharCat ::
  HSt.MonadHexState m => ByteString -> m (Maybe (RawCharCat, ByteString))
extractCharCat xs = runMaybeT $ do
  (n1, rest1) <- MaybeT $ pure $ BS.uncons xs
  -- Next two characters must be identical, and have category
  -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
  cat1 <- lift $ HSt.getHexCode Code.CCatCodeType (Code.CharCode n1)
  let normal = (RawCharCat (Code.CharCode n1) cat1, rest1)
  case cat1 of
    Code.CoreCatCode Code.Superscript -> case BS.uncons rest1 of
      Just (n2, rest2) | n1 == n2 ->
        case BS.uncons rest2 of
          Just (n3, rest3) ->
            lift (HSt.getHexCode Code.CCatCodeType (Code.CharCode n3)) >>= \case
              Code.EndOfLine ->
                pure normal
              _ -> do
                let char3Triod = Code.CharCode $ if n3 < 64 then n3 + 64 else n3 - 64
                cat3 <- lift $ HSt.getHexCode Code.CCatCodeType char3Triod
                pure (RawCharCat char3Triod cat3, rest3)
          Nothing ->
            pure normal
      _ ->
        pure normal
    _ ->
      pure normal

newtype MonadCharCatSourceT m a = MonadCharCatSourceT {unMonadCharCatSourceT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadError e,
      HSt.MonadHexState,
      MonadHexLog
    )

instance
  ( Monad (MonadCharCatSourceT m),
    MonadState st (MonadCharCatSourceT m),
    HasType ByteString st,
    HSt.MonadHexState (MonadCharCatSourceT m),
    Log.MonadHexLog (MonadCharCatSourceT m)
  ) =>
  MonadCharCatSource (MonadCharCatSourceT m)
  where
  -- Lift 'extractCharCat' into a stateful context.
  getCharCat =
    use (typed @ByteString) >>= extractCharCat >>= \case
      Nothing -> pure Nothing
      Just (c, newBS) -> do
        Log.log $ "Fetched raw-char-cat: " <> F.sformat fmtRawCharCat c
        assign' (typed @ByteString) newBS
        pure $ Just c

  peekCharCat =
    use (typed @ByteString) >>= extractCharCat >>= \case
      Nothing -> pure Nothing
      Just (c, _newBS) ->
        pure $ Just c
