module Hex.Common.HexInput.Impl.Categorise where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString qualified as BS
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.HexInput.Impl.CharSource (HexLine (..))
import Hex.Common.HexInput.Impl.CharSource qualified as CharSource
import Hex.Common.HexInput.Impl.CharSourceStack (CharSourceStack (..))
import Hex.Common.HexInput.Impl.CharSourceStack qualified as CharSourceStack
import Hex.Common.HexState.Interface (EHexState)
import Hex.Common.HexState.Interface qualified as HSt
import Hexlude

data RawCharCat = RawCharCat
  { rawCCChar :: Code.CharCode,
    rawCCCat :: Code.CatCode
  }
  deriving stock (Show, Eq, Generic)

fmtRawCharCat :: Fmt RawCharCat
fmtRawCharCat =
  let f1 = F.accessed (.rawCCChar) Code.fmtCharCode
      f2 = F.accessed (.rawCCCat) Code.fmtCatCode
   in F.parenthesised $ f1 <> F.fconst ", " <> f2

extractCharCatFromHexLinePure ::
  EHexState :> es => HexLine -> Eff es (Maybe (RawCharCat, HexLine))
extractCharCatFromHexLinePure (HexLine xs) = runMaybeT $ do
  (n1, rest1) <- MaybeT $ pure $ BS.uncons xs
  -- Next two characters must be identical, and have category
  -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
  cat1 <- lift $ HSt.getHexCode Code.CCatCodeType (Code.CharCode n1)
  let normal = (RawCharCat (Code.CharCode n1) cat1, HexLine rest1)
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
                pure (RawCharCat char3Triod cat3, HexLine rest3)
          Nothing ->
            pure normal
      _ ->
        pure normal
    _ ->
      pure normal

extractCharCatFromCurrentLine ::
  [State CharSourceStack, EHexState] :>> es =>
  Eff es (Maybe RawCharCat)
extractCharCatFromCurrentLine = do
  use @CharSourceStack (CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #currentLine) >>= extractCharCatFromHexLinePure >>= \case
    Nothing ->
      pure Nothing
    Just (charCat, restOfLine) -> do
      assign @CharSourceStack (CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #currentLine) restOfLine
      pure $ Just charCat

peekCharCatOnCurrentLineImpl ::
  [State CharSourceStack, EHexState] :>> es =>
  Eff es (Maybe RawCharCat)
peekCharCatOnCurrentLineImpl =
  use @CharSourceStack (CharSourceStack.currentSourceLens % #workingLine % #sourceLine % #currentLine) >>= extractCharCatFromHexLinePure <&> \case
    Nothing ->
      Nothing
    Just (charCat, _restOfLine) ->
      Just charCat
