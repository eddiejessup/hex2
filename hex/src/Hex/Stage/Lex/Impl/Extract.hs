module Hex.Stage.Lex.Impl.Extract where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Categorise.Impl qualified as Cat
import Hex.Stage.Categorise.Interface qualified as Cat
import Hex.Stage.Lex.Interface.Extract (LexCharCat (..), LexError (..), LexState (..), LexToken (..), mkControlSequence, parToken)
import Hexlude

spaceTok :: LexToken
spaceTok = CharCatLexToken $ LexCharCat (Code.Chr_ ' ') Code.Space

-- Take letters until we see end-of-input, or a non-letter. Leave the non-letter in the rest-of-input
getLetterChars :: HSt.MonadHexState m => ByteString -> m (Seq Code.CharCode, ByteString)
getLetterChars = go Empty
  where
    go acc cs =
      Cat.extractCharCat cs >>= \case
        -- No input left: Return our accumulation, and an empty rest-of-input.
        Nothing ->
          pure (acc, mempty)
        -- See a letter: append to our accumulation, and recur.
        Just (Cat.RawCharCat char (Code.CoreCatCode Code.Letter), csRest) ->
          go (acc :|> char) csRest
        -- See something else: return our accumulation, and return the
        -- rest-of-input *before the fetch* (note, cs, not csRest)
        Just _ ->
          pure (acc, cs)

dropTilEndOfLine :: HSt.MonadHexState m => ByteString -> m ByteString
dropTilEndOfLine xs = do
  Cat.extractCharCat xs >>= \case
    Nothing ->
      pure mempty
    Just (Cat.RawCharCat _ Code.EndOfLine, xsRest) ->
      pure xsRest
    Just (_, xsRest) ->
      dropTilEndOfLine xsRest

extractToken ::
  forall e m.
  (HSt.MonadHexState m, AsType LexError e) =>
  LexState ->
  ByteString ->
  ExceptT e m (Maybe (LexToken, LexState, ByteString))
extractToken = go
  where
    go :: LexState -> ByteString -> ExceptT e m (Maybe (LexToken, LexState, ByteString))
    go _state cs = do
      lift (Cat.extractCharCat cs) >>= \case
        Nothing -> pure Nothing
        Just (Cat.RawCharCat n1 cat1, rest) ->
          case (cat1, _state) of
            -- Control sequence: Grab it.
            (Code.Escape, _) -> do
              lift (Cat.extractCharCat rest) >>= \case
                Nothing ->
                  throwE $ injectTyped TerminalEscapeCharacter
                Just (Cat.RawCharCat csChar1 ctrlSeqCat1, restPostEscape) -> do
                  (ctrlSeqChars, restPostCtrlSeq) <- case ctrlSeqCat1 of
                    Code.CoreCatCode Code.Letter -> do
                      (ctrlWordCharsPostFirst, restPostCtrlWord) <- lift $ getLetterChars restPostEscape
                      pure (csChar1 :<| ctrlWordCharsPostFirst, restPostCtrlWord)
                    _ ->
                      pure (singleton csChar1, restPostEscape)
                  let nextState = case ctrlSeqCat1 of
                        Code.CoreCatCode Code.Space -> SkippingBlanks
                        Code.CoreCatCode Code.Letter -> SkippingBlanks
                        _ -> LineMiddle
                  pure $
                    Just
                      ( ControlSequenceLexToken $ mkControlSequence $ toList ctrlSeqChars,
                        nextState,
                        restPostCtrlSeq
                      )
            -- Comment: Ignore rest of line and switch to line-begin.
            (Code.Comment, _) -> do
              restPostComment <- lift $ dropTilEndOfLine rest
              go LineBegin restPostComment
            -- Empty line: Make a paragraph.
            (Code.EndOfLine, LineBegin) ->
              pure $ Just (parToken, LineBegin, rest)
            -- End of line in middle of line: Make a space token and go to line begin.
            (Code.EndOfLine, LineMiddle) ->
              pure $ Just (spaceTok, LineBegin, rest)
            -- Space in middle of line: Make a space token and start skipping blanks.
            (Code.CoreCatCode Code.Space, LineMiddle) ->
              pure $ Just (spaceTok, SkippingBlanks, rest)
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
              throwE $ injectTyped InvalidCharacter
            -- Simple tokeniser cases.
            --------------------------
            (Code.CoreCatCode cc, _) ->
              pure $ Just (CharCatLexToken $ LexCharCat n1 cc, LineMiddle, rest)
