module Hex.Stage.Parse.Impl.Parsers.Command.Stream where

import ASCII qualified
import Control.Monad.Combinators qualified as PC
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Common.Token.Resolved.Primitive qualified as T
import Hex.Stage.Expand.Interface (PrimTokenSource (..))
import Hex.Stage.Expand.Interface qualified as Par
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

parseOpenFileStream :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => AST.FileStreamType -> Eff es AST.FileStreamModificationCommand
parseOpenFileStream fileStreamType =
  do
    (n, fileName) <- parseXEqualsY Expanding Par.parseInt parseFileName
    pure $ AST.FileStreamModificationCommand fileStreamType (AST.Open fileName) n

headToParseOpenOutput :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => AST.WritePolicy -> T.PrimitiveToken -> Eff es AST.FileStreamModificationCommand
headToParseOpenOutput writePolicy = \case
  T.OpenOutputTok ->
    parseOpenFileStream (AST.FileOutput writePolicy)
  t ->
    Par.parseFail $ "headToParseOpenOutput " <> F.sformat PT.fmtPrimitiveToken t

headToParseCloseOutput :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => AST.WritePolicy -> T.PrimitiveToken -> Eff es AST.FileStreamModificationCommand
headToParseCloseOutput writePolicy = \case
  T.CloseOutputTok ->
    AST.FileStreamModificationCommand (AST.FileOutput writePolicy) AST.Close <$> Par.parseInt
  t ->
    Par.parseFail $ "headToParseOpenOutput " <> F.sformat PT.fmtPrimitiveToken t

headToParseWriteToStream :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => AST.WritePolicy -> T.PrimitiveToken -> Eff es AST.StreamWriteCommand
headToParseWriteToStream writePolicy = \case
  T.WriteTok ->
    do
      n <- Par.parseInt
      txt <- case writePolicy of
        AST.Immediate -> AST.ImmediateWriteText <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
        AST.Deferred -> AST.DeferredWriteText <$> Par.parseInhibitedGeneralText Par.ExpectingBeginGroup
      pure $ AST.StreamWriteCommand n txt
  t ->
    Par.parseFail $ "headToParseOpenOutput " <> F.sformat PT.fmtPrimitiveToken t

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es HexFilePath
parseFileName = do
  skipOptionalSpaces Expanding
  fileNameAsciiChars <-
    PC.some $
      satisfyCharCatThen Expanding $ \cc ->
        Code.codeAsAsciiChar
          <$> ( case cc of
                  -- Check that the char-cat has the right properties.
                  LT.LexCharCat c Code.Letter ->
                    Just c
                  LT.LexCharCat c Code.Other
                    | isValidOther c ->
                        Just c
                  _ ->
                    Nothing
              )
  skipSatisfied (satisfyCharCatThen Expanding) charCatIsSpace
  pure $ HexFilePath $ ASCII.charListToUnicodeString fileNameAsciiChars
  where
    isValidOther = \case
      -- Not in the spec, but let's say these are OK.
      Code.Chr_ '/' -> True
      Code.Chr_ '.' -> True
      Code.Chr_ '_' -> True
      -- 'Other' Characters for decimal digits are OK.
      cc ->
        H.Ascii.isDecDigit $ cc ^. typed @Word8
