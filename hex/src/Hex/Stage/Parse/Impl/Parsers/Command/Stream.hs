module Hex.Stage.Parse.Impl.Parsers.Command.Stream where

import ASCII qualified
import Control.Monad.Combinators qualified as PC
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hex.Common.Parse (MonadPrimTokenParse (..))
import Hex.Stage.Interpret.Build.Box.Elem qualified as Box
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude
import qualified Hex.Common.HexState.Interface.Resolve.PrimitiveToken as PT

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
parseFileName :: MonadPrimTokenParse m => m Box.HexFilePath
parseFileName = do
  Par.skipOptionalSpaces
  fileNameAsciiChars <-
    PC.some $
      satisfyThen $ \pt -> do
        -- First check that we get a char-cat (with ^?)
        -- and that it has the right properties.
        code <-
          pt ^? PT.primTokCharCat >>= \case
            Lex.LexCharCat c Code.Letter ->
              Just c
            Lex.LexCharCat c Code.Other
              | isValidOther c ->
                  Just c
            _ ->
              Nothing
        pure $ Code.codeAsAsciiChar code
  Par.skipSatisfied Par.isSpace
  pure $ Box.HexFilePath $ ASCII.charListToUnicodeString fileNameAsciiChars
  where
    isValidOther = \case
      -- Not in the spec, but let's say these are OK.
      Code.Chr_ '/' -> True
      Code.Chr_ '.' -> True
      Code.Chr_ '_' -> True
      -- 'Other' Characters for decimal digits are OK.
      cc ->
        H.Ascii.isDecDigit $ cc ^. typed @Word8
