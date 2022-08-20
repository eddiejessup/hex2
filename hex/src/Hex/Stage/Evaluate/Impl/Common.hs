module Hex.Stage.Evaluate.Impl.Common where

import ASCII qualified
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.HexState.Interface.TokenList qualified as LT
import Hex.Common.Token.Lexed qualified as LT
import Hexlude

data EvaluationError
  = ValueNotInRange
  | InvalidTokenInBalancedText LT.ControlSequence
  | InvalidLetterInHyphenationPatterns Code.CharCode
  deriving stock (Show, Generic)

fmtEvaluationError :: Fmt EvaluationError
fmtEvaluationError = F.later $ \case
  ValueNotInRange -> "Value not in range"
  InvalidTokenInBalancedText pt -> "Invalid token in balanced text: " <> F.bformat LT.fmtControlSequence pt
  InvalidLetterInHyphenationPatterns c -> "Found letter with zero lowercase-code in hyphenation patterns: " <> F.bformat Code.fmtCharCode c

evalBalancedTextToText ::
  Error EvaluationError :> es =>
  LT.BalancedText ->
  Eff es Text
evalBalancedTextToText bt = do
  -- For each primitive-token in the expanded-balanced-text.
  msgAsciiChars <- forM bt.unBalancedText $ \lt ->
    -- Get the lex-char-cat from the token, if it is the correct token type.
    case lt of
      -- If it is the wrong type, throw an error.
      LT.ControlSequenceLexToken cs -> throwError $ InvalidTokenInBalancedText cs
      -- Otherwise, convert the char-code to its equivalent ASCII-character.
      LT.CharCatLexToken lexCharCat ->
        pure $ Code.codeAsAsciiChar $ lexCharCat.lexCCChar
  -- Build a text from the list of ASCII-characters.
  pure $ ASCII.charListToText $ toList msgAsciiChars
