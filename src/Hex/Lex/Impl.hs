module Hex.Lex.Impl where

import Data.Text qualified as Tx
import Hex.Categorise.Impl qualified as H.Cat
import Hex.Categorise.Types qualified as H.Cat
import Hex.Codes qualified as Code
import Hex.Lex.Types
import Hex.MonadHexState.Interface qualified as H.St
import Hexlude

spaceTok :: LexToken
spaceTok = CharCatLexToken $ LexCharCat (Code.Chr_ ' ') Code.Space

-- Take letters until we see end-of-input, or a non-letter. Leave the non-letter in the rest-of-input
getLetterChars :: H.St.MonadHexState m => ByteString -> m (Seq Code.CharCode, ByteString)
getLetterChars = go Empty
  where
    go acc cs =
      runExceptT @H.Cat.CatFailure (H.Cat.extractCharCat cs) >>= \case
        -- No input left: Return our accumulation, and an empty rest-of-input.
        Left (H.Cat.CatEndOfInputFailure H.Cat.EndOfInput) ->
          pure (acc, mempty)
        -- See a letter: append to our accumulation, and recur.
        Right (H.Cat.RawCharCat char (Code.CoreCatCode Code.Letter), csRest) ->
          go (acc :|> char) csRest
        -- See something else: return our accumulation, and return the
        -- rest-of-input *before the fetch* (note, cs, not csRest)
        Right _ ->
          pure (acc, cs)

dropTilEndOfLine :: H.St.MonadHexState m => ByteString -> m ByteString
dropTilEndOfLine xs = do
  runExceptT @H.Cat.CatFailure (H.Cat.extractCharCat xs) >>= \case
    Left (H.Cat.CatEndOfInputFailure H.Cat.EndOfInput) ->
      pure mempty
    Right (H.Cat.RawCharCat _ Code.EndOfLine, xsRest) ->
      pure xsRest
    Right (_, xsRest) ->
      dropTilEndOfLine xsRest

extractToken ::
  forall e m.
  (H.St.MonadHexState m, MonadError e m, AsType H.Cat.EndOfInput e, AsType LexError e) =>
  LexState ->
  ByteString ->
  m (LexToken, LexState, ByteString)
extractToken = go
  where
    go :: LexState -> ByteString -> m (LexToken, LexState, ByteString)
    go _state cs = do
      (H.Cat.RawCharCat n1 cat1, rest) <- H.Cat.extractCharCat cs
      case (cat1, _state) of
        -- Control sequence: Grab it.
        (Code.Escape, _) -> do
          runExceptT @H.Cat.CatFailure (H.Cat.extractCharCat rest) >>= \case
            Left (H.Cat.CatEndOfInputFailure H.Cat.EndOfInput) ->
              throwError $ injectTyped TerminalEscapeCharacter
            Right (H.Cat.RawCharCat csChar1 ctrlSeqCat1, restPostEscape) -> do
              (ctrlSeqChars, restPostCtrlSeq) <- case ctrlSeqCat1 of
                Code.CoreCatCode Code.Letter -> do
                  (ctrlWordCharsPostFirst, restPostCtrlWord) <- getLetterChars restPostEscape
                  pure (csChar1 :<| ctrlWordCharsPostFirst, restPostCtrlWord)
                _ ->
                  pure (singleton csChar1, restPostEscape)
              let nextState = case ctrlSeqCat1 of
                    Code.CoreCatCode Code.Space -> SkippingBlanks
                    Code.CoreCatCode Code.Letter -> SkippingBlanks
                    _ -> LineMiddle
              pure
                ( ControlSequenceLexToken $ mkControlSequence $ toList ctrlSeqChars,
                  nextState,
                  restPostCtrlSeq
                )
        -- Comment: Ignore rest of line and switch to line-begin.
        (Code.Comment, _) -> do
          restPostComment <- dropTilEndOfLine rest
          go LineBegin restPostComment
        -- Empty line: Make a paragraph.
        (Code.EndOfLine, LineBegin) ->
          pure (parToken, LineBegin, rest)
        -- End of line in middle of line: Make a space token and go to line begin.
        (Code.EndOfLine, LineMiddle) ->
          pure (spaceTok, LineBegin, rest)
        -- Space in middle of line: Make a space token and start skipping blanks.
        (Code.CoreCatCode Code.Space, LineMiddle) ->
          pure (spaceTok, SkippingBlanks, rest)
        -- Ignore cases
        ---------------
        -- Space at the start of a line.
        (Code.CoreCatCode Code.Space, LineBegin) ->
          go _state rest
        -- Space or end of line, while skipping blanks.
        (Code.CoreCatCode Code.Space, SkippingBlanks) ->
          go _state rest
        (Code.EndOfLine, SkippingBlanks) ->
          go _state rest
        -- Ignored.
        (Code.Ignored, _) ->
          go _state rest
        -- Error cases
        --------------
        -- Invalid: TeXbook says to print an error message.
        (Code.Invalid, _) ->
          throwError $ injectTyped InvalidCharacter
        -- Simple tokeniser cases.
        --------------------------
        (Code.CoreCatCode cc, _) ->
          pure (CharCatLexToken $ LexCharCat n1 cc, LineMiddle, rest)

charsToLexTokens :: forall m. H.St.MonadHexState m => ByteString -> ExceptT LexError m [LexToken]
charsToLexTokens = go LineBegin
  where
    go lexState xs =
      runExceptT (extractToken lexState xs) >>= \case
        Left (LexEndOfInputFailure H.Cat.EndOfInput) ->
          pure []
        Left (LexErrorFailure le) ->
          throwError le
        Right (tok, lexState1, xs1) -> do
          v <- go lexState1 xs1
          pure $ tok : v

-- usableCharsToLexTokens :: ByteString -> Either LexError [LexToken]
-- usableCharsToLexTokens = charsToLexTokens Code.usableCatLookup

renderLexResult :: Either LexError [LexToken] -> Text
renderLexResult = \case
  Left le -> "Ended with error: " <> show le
  Right xs -> Tx.intercalate "\n" $ show <$> xs
