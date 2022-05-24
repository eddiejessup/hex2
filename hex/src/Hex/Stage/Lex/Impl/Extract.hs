module Hex.Stage.Lex.Impl.Extract where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Categorise.Interface qualified as Cat
import Hex.Stage.Lex.Interface.Extract (LexCharCat (..), LexError (..), LexState (..), LexToken (..), mkControlSequence, parToken)
import Hexlude

spaceTok :: LexToken
spaceTok = CharCatLexToken $ LexCharCat (Code.Chr_ ' ') Code.Space

-- Take letters until we see end-of-input, or a non-letter. Leave the non-letter in the rest-of-input
getLetterChars :: Cat.MonadCharCatSource m => m (Seq Code.CharCode)
getLetterChars = go Empty
  where
    go acc =
      Cat.peekCharCat >>= \case
        -- No input left: Return our accumulation, and an empty rest-of-input.
        Nothing ->
          pure acc
        -- See a letter: append to our accumulation, and recur.
        Just (Cat.RawCharCat char (Code.CoreCatCode Code.Letter)) -> do
          void $ Cat.getCharCat
          go (acc :|> char)
        -- See something else: return our accumulation, and return the
        -- rest-of-input *before the fetch* (note, cs, not csRest)
        Just _ -> do
          pure acc

dropTilEndOfLine :: Cat.MonadCharCatSource m => m ()
dropTilEndOfLine =
  Cat.getCharCat >>= \case
    Just (Cat.RawCharCat _ Code.EndOfLine) ->
      pure ()
    _ ->
      dropTilEndOfLine

extractToken ::
  forall e m.
  (HSt.MonadHexState m, AsType LexError e, Cat.MonadCharCatSource m) =>
  LexState ->
  ExceptT e m (Maybe (LexToken, LexState))
extractToken = go
  where
    go :: LexState -> ExceptT e m (Maybe (LexToken, LexState))
    go _state = do
      lift Cat.getCharCat >>= \case
        Nothing -> pure Nothing
        Just (Cat.RawCharCat n1 cat1) ->
          case (cat1, _state) of
            -- Control sequence: Grab it.
            (Code.Escape, _) -> do
              lift Cat.getCharCat >>= \case
                Nothing ->
                  throwE $ injectTyped TerminalEscapeCharacter
                Just (Cat.RawCharCat csChar1 ctrlSeqCat1) -> do
                  ctrlSeqChars <- case ctrlSeqCat1 of
                    Code.CoreCatCode Code.Letter -> do
                      (ctrlWordCharsPostFirst) <- lift getLetterChars
                      pure (csChar1 :<| ctrlWordCharsPostFirst)
                    _ ->
                      pure (singleton csChar1)
                  let nextState = case ctrlSeqCat1 of
                        Code.CoreCatCode Code.Space -> SkippingBlanks
                        Code.CoreCatCode Code.Letter -> SkippingBlanks
                        _ -> LineMiddle
                  pure $
                    Just
                      ( ControlSequenceLexToken $ mkControlSequence $ toList ctrlSeqChars,
                        nextState
                      )
            -- Comment: Ignore rest of line and switch to line-begin.
            (Code.Comment, _) -> do
              lift $ dropTilEndOfLine
              go LineBegin
            -- Empty line: Make a paragraph.
            (Code.EndOfLine, LineBegin) ->
              pure $ Just (parToken, LineBegin)
            -- End of line in middle of line: Make a space token and go to line begin.
            (Code.EndOfLine, LineMiddle) ->
              pure $ Just (spaceTok, LineBegin)
            -- Space in middle of line: Make a space token and start skipping blanks.
            (Code.CoreCatCode Code.Space, LineMiddle) ->
              pure $ Just (spaceTok, SkippingBlanks)
            -- Ignore cases
            ---------------
            -- Space at the start of a line.
            (Code.CoreCatCode Code.Space, LineBegin) ->
              go _state
            -- Space or end of line, while skipping blanks.
            (Code.CoreCatCode Code.Space, SkippingBlanks) ->
              go _state
            (Code.EndOfLine, SkippingBlanks) ->
              go _state
            -- Ignored.
            (Code.Ignored, _) ->
              go _state
            -- Error cases
            --------------
            -- Invalid: TeXbook says to print an error message.
            (Code.Invalid, _) ->
              throwE $ injectTyped InvalidCharacter
            -- Simple tokeniser cases.
            --------------------------
            (Code.CoreCatCode cc, _) ->
              pure $ Just (CharCatLexToken $ LexCharCat n1 cc, LineMiddle)
