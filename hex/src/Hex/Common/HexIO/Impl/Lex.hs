module Hex.Common.HexIO.Impl.Lex where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexIO.Impl.Categorise qualified as Cat
import Hex.Common.HexIO.Impl.CharSource qualified as HIO
import Hex.Common.HexIO.Impl.IOState (IOState)
import Hex.Common.HexIO.Interface qualified as HIO
import Hex.Common.HexState.Interface (EHexState)
import Hex.Common.Token.Lexed (LexCharCat (..), LexToken (..))
import Hex.Common.Token.Lexed qualified as LT
import Hexlude

spaceTok :: LexToken
spaceTok = CharCatLexToken $ LexCharCat (Code.Chr_ ' ') Code.Space

-- Take letters until we see end-of-input, or a non-letter. Leave the non-letter in the rest-of-input
getLetterChars ::
  (State IOState :> es, EHexState :> es) =>
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
  (EHexState :> es, Error HIO.LexError :> es, State IOState :> es) =>
  HIO.LineState ->
  Eff es (Maybe (LexToken, HIO.LineState))
extractLexTokenFromSourceLine = go
  where
    go :: HIO.LineState -> Eff es (Maybe (LexToken, HIO.LineState))
    go _state = do
      Cat.extractCharCatFromCurrentLine >>= \case
        Nothing -> pure Nothing
        Just (Cat.RawCharCat n1 cat1) ->
          case (cat1, _state) of
            -- Control sequence: Grab it.
            (Code.Escape, _) -> do
              Cat.extractCharCatFromCurrentLine >>= \case
                Nothing ->
                  throwError HIO.TerminalEscapeCharacter
                Just (Cat.RawCharCat csChar1 ctrlSeqCat1) -> do
                  ctrlSeqChars <- case ctrlSeqCat1 of
                    Code.CoreCatCode Code.Letter -> do
                      (ctrlWordCharsPostFirst) <- getLetterChars
                      pure (csChar1 :<| ctrlWordCharsPostFirst)
                    _ ->
                      pure (singleton csChar1)
                  let nextState = case ctrlSeqCat1 of
                        Code.CoreCatCode Code.Space -> HIO.SkippingBlanks
                        Code.CoreCatCode Code.Letter -> HIO.SkippingBlanks
                        _ -> HIO.LineMiddle
                  pure $
                    Just
                      ( ControlSequenceLexToken $ LT.mkControlSequence $ toList ctrlSeqChars,
                        nextState
                      )
            -- Comment: Ignore rest of line and switch to line-begin.
            (Code.Comment, _) -> do
              pure Nothing
            -- Empty line: Make a paragraph.
            (Code.EndOfLine, HIO.LineBegin) ->
              pure $ Just (LT.parToken, HIO.LineBegin)
            -- End of line in middle of line: Make a space token and go to line begin.
            (Code.EndOfLine, HIO.LineMiddle) ->
              pure $ Just (spaceTok, HIO.LineBegin)
            -- Space in middle of line: Make a space token and start skipping blanks.
            (Code.CoreCatCode Code.Space, HIO.LineMiddle) ->
              pure $ Just (spaceTok, HIO.SkippingBlanks)
            -- Ignore cases
            ---------------
            -- Space at the start of a line.
            (Code.CoreCatCode Code.Space, HIO.LineBegin) ->
              go _state
            -- Space or end of line, while skipping blanks.
            (Code.CoreCatCode Code.Space, HIO.SkippingBlanks) ->
              go _state
            (Code.EndOfLine, HIO.SkippingBlanks) ->
              go _state
            -- Ignored.
            (Code.Ignored, _) ->
              go _state
            -- Error cases
            --------------
            -- Invalid: TeXbook says to print an error message.
            (Code.Invalid, _) ->
              throwError HIO.InvalidCharacter
            -- Simple tokeniser cases.
            --------------------------
            (Code.CoreCatCode cc, _) ->
              pure $ Just (CharCatLexToken $ LexCharCat n1 cc, HIO.LineMiddle)
