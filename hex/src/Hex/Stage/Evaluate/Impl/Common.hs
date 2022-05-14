module Hex.Stage.Evaluate.Impl.Common where

import ASCII qualified
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.HexState.Interface.TokenList qualified as LT
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Lex.Interface.Extract qualified as PT
import Hexlude

data EvaluationError
  = ValueNotInRange
  | InvalidTokenInBalancedText Lex.ControlSequence
  deriving stock (Show, Generic)

fmtEvaluationError :: Fmt EvaluationError
fmtEvaluationError = F.later $ \case
  ValueNotInRange -> "Value not in range"
  InvalidTokenInBalancedText pt -> "Invalid token in balanced text: " <> F.bformat PT.fmtControlSequence pt

evalBalancedTextToText ::
  (MonadError e m, AsType EvaluationError e) =>
  LT.BalancedText ->
  m Text
evalBalancedTextToText bt = do
  -- For each primitive-token in the expanded-balanced-text.
  msgAsciiChars <- forM bt.unBalancedText $ \lt ->
    -- Get the lex-char-cat from the token, if it is the correct token type.
    case lt of
      -- If it is the wrong type, throw an error.
      Lex.ControlSequenceLexToken cs -> throwError $ injectTyped $ InvalidTokenInBalancedText cs
      -- Otherwise, convert the char-code to its equivalent ASCII-character.
      Lex.CharCatLexToken lexCharCat ->
        pure $ Code.codeAsAsciiChar $ lexCharCat.lexCCChar
  -- Build a text from the list of ASCII-characters.
  pure $ ASCII.charListToText $ toList msgAsciiChars

evalExpandedBalancedTextToText ::
  (MonadError e m, AsType EvaluationError e) =>
  ST.ExpandedBalancedText ->
  m Text
evalExpandedBalancedTextToText ebt = evalBalancedTextToText (ebt.unExpandedBalancedText)
