module Hex.Stage.Parse.Impl.Parsers.Command.Stream where

import ASCII qualified
import Control.Monad.Combinators qualified as PC
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as H.C
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Common.Parse (MonadPrimTokenParse(..))

parseOpenFileStream :: MonadPrimTokenParse m => AST.FileStreamType -> m AST.FileStreamModificationCommand
parseOpenFileStream fileStreamType =
  do
    (n, fileName) <- Par.parseXEqualsY Par.parseInt parseFileName
    pure $ AST.FileStreamModificationCommand fileStreamType (AST.Open fileName) n

headToParseOpenOutput :: MonadPrimTokenParse m => AST.WritePolicy -> T.PrimitiveToken -> m AST.FileStreamModificationCommand
headToParseOpenOutput writePolicy = \case
  T.OpenOutputTok ->
    parseOpenFileStream (AST.FileOutput writePolicy)
  _ ->
    empty

headToParseCloseOutput :: MonadPrimTokenParse m => AST.WritePolicy -> T.PrimitiveToken -> m AST.FileStreamModificationCommand
headToParseCloseOutput writePolicy = \case
  T.CloseOutputTok ->
    AST.FileStreamModificationCommand (AST.FileOutput writePolicy) AST.Close <$> Par.parseInt
  _ ->
    empty

headToParseWriteToStream :: MonadPrimTokenParse m => AST.WritePolicy -> T.PrimitiveToken -> m AST.StreamWriteCommand
headToParseWriteToStream writePolicy = \case
  T.WriteTok ->
    do
      n <- Par.parseInt
      txt <- case writePolicy of
        AST.Immediate -> AST.ImmediateWriteText <$> Par.parseExpandedGeneralText
        AST.Deferred -> AST.DeferredWriteText <$> Par.parseInhibitedGeneralText
      pure $ AST.StreamWriteCommand n txt
  _ ->
    empty

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: MonadPrimTokenParse m => m AST.HexFilePath
parseFileName = do
  Par.skipOptionalSpaces
  fileNameAsciiChars <-
    PC.some $
      satisfyThen $ \pt -> do
        code <-
          pt ^? Par.primTokCharCat >>= \case
            Lex.LexCharCat c H.C.Letter ->
              Just c
            Lex.LexCharCat c H.C.Other
              | isValidOther c ->
                Just c
            _ ->
              Nothing
        code ^. typed @Word8 % to ASCII.word8ToCharMaybe
  Par.skipSatisfied Par.isSpace
  pure $ AST.HexFilePath $ ASCII.charListToUnicodeString fileNameAsciiChars
  where
    isValidOther = \case
      -- Not in the spec, but let's say these are OK.
      H.C.Chr_ '/' -> True
      H.C.Chr_ '.' -> True
      H.C.Chr_ '_' -> True
      -- 'Other' Characters for decimal digits are OK.
      cc ->
        H.Ascii.isDecDigit $ cc ^. typed @Word8
