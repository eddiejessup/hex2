module Hex.Parse.Parsers.Command.Stream where

import ASCII qualified
import Control.Monad.Combinators qualified as PC
import Hex.Ascii qualified as H.Ascii
import Hex.Codes qualified as H.C
import Hex.Lex.Types qualified as H.Lex
import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Parse.Syntax.Command qualified as H.Par.Syn ()
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.BalancedText qualified as Par
import Hex.Parse.Parsers.Combinators qualified as Par
import Hex.Parse.Parsers.Quantity.Number qualified as Par
import Hex.Symbol.Token.Primitive qualified as T
import Hexlude

parseOpenFileStream :: MonadPrimTokenSource m => H.Syn.FileStreamType -> m (H.Syn.FileStreamModificationCommand 'H.Syn.Parsed)
parseOpenFileStream fileStreamType =
  do
    (n, fileName) <- Par.parseXEqualsY Par.parseInt parseFileName
    pure $ H.Syn.FileStreamModificationCommand fileStreamType (H.Syn.Open fileName) n

headToParseOpenOutput :: MonadPrimTokenSource m => H.Syn.WritePolicy -> T.PrimitiveToken -> m (H.Syn.FileStreamModificationCommand 'H.Syn.Parsed)
headToParseOpenOutput writePolicy = \case
  T.OpenOutputTok ->
    parseOpenFileStream (H.Syn.FileOutput writePolicy)
  _ ->
    empty

headToParseCloseOutput :: MonadPrimTokenSource m => H.Syn.WritePolicy -> T.PrimitiveToken -> m (H.Syn.FileStreamModificationCommand 'H.Syn.Parsed)
headToParseCloseOutput writePolicy = \case
  T.CloseOutputTok ->
    H.Syn.FileStreamModificationCommand (H.Syn.FileOutput writePolicy) H.Syn.Close <$> Par.parseInt
  _ ->
    empty

headToParseWriteToStream :: MonadPrimTokenSource m => H.Syn.WritePolicy -> T.PrimitiveToken -> m (H.Syn.StreamWriteCommand 'H.Syn.Parsed)
headToParseWriteToStream writePolicy = \case
  T.WriteTok ->
    do
      n <- Par.parseInt
      txt <- case writePolicy of
        H.Syn.Immediate -> H.Syn.ImmediateWriteText <$> Par.parseExpandedGeneralText
        H.Syn.Deferred -> H.Syn.DeferredWriteText <$> Par.parseInhibitedGeneralText
      pure $ H.Syn.StreamWriteCommand n txt
  _ ->
    empty

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: MonadPrimTokenSource m => m H.Syn.HexFilePath
parseFileName = do
  Par.skipOptionalSpaces
  fileNameAsciiChars <-
    PC.some $
      satisfyThen $ \pt -> do
        code <-
          pt ^? Par.primTokCharCat >>= \case
            H.Lex.LexCharCat c H.C.Letter ->
              Just c
            H.Lex.LexCharCat c H.C.Other
              | isValidOther c ->
                Just c
            _ ->
              Nothing
        code ^. typed @Word8 % to ASCII.word8ToCharMaybe
  Par.skipSatisfied Par.isSpace
  pure $ H.Syn.HexFilePath $ ASCII.charListToUnicodeString fileNameAsciiChars
  where
    isValidOther = \case
      -- Not in the spec, but let's say these are OK.
      H.C.Chr_ '/' -> True
      H.C.Chr_ '.' -> True
      H.C.Chr_ '_' -> True
      -- 'Other' Characters for decimal digits are OK.
      cc ->
        H.Ascii.isDecDigit $ cc ^. typed @Word8
