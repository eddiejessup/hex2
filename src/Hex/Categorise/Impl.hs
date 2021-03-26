module Hex.Categorise.Impl where

import Data.ByteString qualified as BS
import Formatting qualified as F
import Hex.Categorise.Types
import Hex.Codes qualified as Code
import Hex.MonadHexState.Interface qualified as H.St
import Hexlude

extractCharCat ::
  (H.St.MonadHexState m, MonadError e m, AsType EndOfInput e) => ByteString -> m (RawCharCat, ByteString)
extractCharCat xs = do
  (n1, rest1) <- note (injectTyped EndOfInput) (BS.uncons xs)
  -- Next two characters must be identical, and have category
  -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
  cat1 <- H.St.getCategory (Code.CharCode n1)
  let normal = (RawCharCat (Code.CharCode n1) cat1, rest1)
  case cat1 of
    Code.CoreCatCode Code.Superscript -> case BS.uncons rest1 of
      Just (n2, rest2) | n1 == n2 ->
        case BS.uncons rest2 of
          Just (n3, rest3) ->
            H.St.getCategory (Code.CharCode n3) >>= \case
              Code.EndOfLine ->
                pure normal
              _ -> do
                let char3Triod = Code.CharCode $ if n3 < 64 then n3 + 64 else n3 - 64
                cat3 <- H.St.getCategory char3Triod
                pure (RawCharCat char3Triod cat3, rest3)
          Nothing ->
            pure normal
      _ ->
        pure normal
    _ ->
      pure normal

charsToCharCats :: forall m. H.St.MonadHexState m => ByteString -> m [RawCharCat]
charsToCharCats = go
  where
    -- Just specialising to make it clearer what's going on monad-wise.
    extractCharCatMono :: ByteString -> ExceptT CatFailure m (RawCharCat, ByteString)
    extractCharCatMono xs = extractCharCat xs

    go :: ByteString -> m [RawCharCat]
    go xs =
      runExceptT (extractCharCatMono xs) >>= \case
        Left (CatEndOfInputFailure EndOfInput) ->
          pure []
        Right (cc, xs1) -> do
          v <- go xs1
          pure $ cc : v

-- usableCharsToCharCats :: ByteString -> [RawCharCat]
-- usableCharsToCharCats = charsToCharCats Code.usableCatLookup

fmtCategoriseResult :: Fmt [RawCharCat] r
fmtCategoriseResult = F.unlined F.shown
