module Hex.Stage.Parse.Impl.Parsers.Command.Stream where

import ASCII qualified
import Control.Monad.Combinators qualified as PC
import Formatting qualified as F
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Common.Quantity.Common qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Common.Token.Resolved.Primitive qualified as T
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

parseOpenFileStream :: MonadPrimTokenParse m => AST.FileStreamType -> m AST.FileStreamModificationCommand
parseOpenFileStream fileStreamType =
  do
    (n, fileName) <- parseXEqualsY Expanding Par.parseInt parseFileName
    pure $ AST.FileStreamModificationCommand fileStreamType (AST.Open fileName) n

headToParseOpenOutput :: MonadPrimTokenParse m => AST.WritePolicy -> T.PrimitiveToken -> m AST.FileStreamModificationCommand
headToParseOpenOutput writePolicy = \case
  T.OpenOutputTok ->
    parseOpenFileStream (AST.FileOutput writePolicy)
  t ->
    Par.parseFailure $ "headToParseOpenOutput " <> F.sformat PT.fmtPrimitiveToken t

headToParseCloseOutput :: MonadPrimTokenParse m => AST.WritePolicy -> T.PrimitiveToken -> m AST.FileStreamModificationCommand
headToParseCloseOutput writePolicy = \case
  T.CloseOutputTok ->
    AST.FileStreamModificationCommand (AST.FileOutput writePolicy) AST.Close <$> Par.parseInt
  t ->
    Par.parseFailure $ "headToParseOpenOutput " <> F.sformat PT.fmtPrimitiveToken t

headToParseWriteToStream :: MonadPrimTokenParse m => AST.WritePolicy -> T.PrimitiveToken -> m AST.StreamWriteCommand
headToParseWriteToStream writePolicy = \case
  T.WriteTok ->
    do
      n <- Par.parseInt
      txt <- case writePolicy of
        AST.Immediate -> AST.ImmediateWriteText <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
        AST.Deferred -> AST.DeferredWriteText <$> Par.parseInhibitedGeneralText Par.ExpectingBeginGroup
      pure $ AST.StreamWriteCommand n txt
  t ->
    Par.parseFailure $ "headToParseOpenOutput " <> F.sformat PT.fmtPrimitiveToken t

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: MonadPrimTokenParse m => m Q.HexFilePath
parseFileName = do
  skipOptionalSpaces Expanding
  fileNameAsciiChars <-
    PC.some $
      satisfyLexThenExpanding $ \lt -> do
        -- First check that we get a char-cat (with ^?)
        -- and that it has the right properties.
        code <-
          lt ^? LT.lexTokCharCat >>= \case
            LT.LexCharCat c Code.Letter ->
              Just c
            LT.LexCharCat c Code.Other
              | isValidOther c ->
                  Just c
            _ ->
              Nothing
        Just $ Code.codeAsAsciiChar code
  skipSatisfied satisfyLexThenExpanding lexTokenIsSpace
  pure $ Q.HexFilePath $ ASCII.charListToUnicodeString fileNameAsciiChars
  where
    isValidOther = \case
      -- Not in the spec, but let's say these are OK.
      Code.Chr_ '/' -> True
      Code.Chr_ '.' -> True
      Code.Chr_ '_' -> True
      -- 'Other' Characters for decimal digits are OK.
      cc ->
        H.Ascii.isDecDigit $ cc ^. typed @Word8
