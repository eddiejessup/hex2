module Hex.Stage.Read.Impl.Lex where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface (EHexState)
import Hex.Common.Token.Lexed (LexCharCat (..), LexToken (..))
import Hex.Common.Token.Lexed qualified as LT
import Hex.Stage.Read.Impl.Categorise qualified as Cat
import Hex.Stage.Read.Impl.CharSource qualified as HIn
import Hex.Stage.Read.Impl.CharSourceStack (CharSourceStack)
import Hex.Stage.Read.Interface qualified as HIn
import Hexlude

spaceTok :: LexToken
spaceTok = CharCatLexToken $ LexCharCat (Code.Chr_ ' ') Code.Space

-- Take letters until we see end-of-input, or a non-letter. Leave the non-letter in the rest-of-input
getLetterChars ::
  [State CharSourceStack, EHexState] :>> es =>
  Eff es (Seq Code.CharCode)
getLetterChars = go Empty
  where
    go acc =
      Cat.peekCharCatOnCurrentLineImpl >>= \case
        -- No input left: Return our accumulation, and an empty rest-of-input.
        Nothing ->
          pure acc
        -- See a letter: append to our accumulation, and recur.
        Just (Cat.RawCharCat char (Code.CoreCatCode Code.Letter)) -> do
          void $ Cat.extractCharCatFromCurrentLine
          go (acc :|> char)
        -- See something else: return our accumulation, and return the
        -- rest-of-input *before the fetch* (note, cs, not csRest)
        Just _ -> do
          pure acc

extractLexTokenFromSourceLine ::
  forall es.
  [EHexState, Error HIn.LexError, State CharSourceStack] :>> es =>
  HIn.LineState ->
  Eff es (Maybe (LexToken, HIn.LineState))
extractLexTokenFromSourceLine = go
  where
    go :: HIn.LineState -> Eff es (Maybe (LexToken, HIn.LineState))
    go _state = do
      Cat.extractCharCatFromCurrentLine >>= \case
        Nothing -> pure Nothing
        Just (Cat.RawCharCat n1 cat1) ->
          case (cat1, _state) of
            -- Control sequence: Grab it.
            (Code.Escape, _) -> do
              Cat.extractCharCatFromCurrentLine >>= \case
                Nothing ->
                  throwError HIn.TerminalEscapeCharacter
                Just (Cat.RawCharCat csChar1 ctrlSeqCat1) -> do
                  ctrlSeqChars <- case ctrlSeqCat1 of
                    Code.CoreCatCode Code.Letter -> do
                      (ctrlWordCharsPostFirst) <- getLetterChars
                      pure (csChar1 :<| ctrlWordCharsPostFirst)
                    _ ->
                      pure (singleton csChar1)
                  let nextState = case ctrlSeqCat1 of
                        Code.CoreCatCode Code.Space -> HIn.SkippingBlanks
                        Code.CoreCatCode Code.Letter -> HIn.SkippingBlanks
                        _ -> HIn.LineMiddle
                  pure $
                    Just
                      ( ControlSequenceLexToken $ LT.mkControlSequence $ toList ctrlSeqChars,
                        nextState
                      )
            -- Comment: Ignore rest of line and switch to line-begin.
            (Code.Comment, _) -> do
              pure Nothing
            -- Empty line: Make a paragraph.
            (Code.EndOfLine, HIn.LineBegin) ->
              pure $ Just (LT.parToken, HIn.LineBegin)
            -- End of line in middle of line: Make a space token and go to line begin.
            (Code.EndOfLine, HIn.LineMiddle) ->
              pure $ Just (spaceTok, HIn.LineBegin)
            -- Space in middle of line: Make a space token and start skipping blanks.
            (Code.CoreCatCode Code.Space, HIn.LineMiddle) ->
              pure $ Just (spaceTok, HIn.SkippingBlanks)
            -- Ignore cases
            ---------------
            -- Space at the start of a line.
            (Code.CoreCatCode Code.Space, HIn.LineBegin) ->
              go _state
            -- Space or end of line, while skipping blanks.
            (Code.CoreCatCode Code.Space, HIn.SkippingBlanks) ->
              go _state
            (Code.EndOfLine, HIn.SkippingBlanks) ->
              go _state
            -- Ignored.
            (Code.Ignored, _) ->
              go _state
            -- Error cases
            --------------
            -- Invalid: TeXbook says to print an error message.
            (Code.Invalid, _) ->
              throwError HIn.InvalidCharacter
            -- Simple tokeniser cases.
            --------------------------
            (Code.CoreCatCode cc, _) ->
              pure $ Just (CharCatLexToken $ LexCharCat n1 cc, HIn.LineMiddle)
