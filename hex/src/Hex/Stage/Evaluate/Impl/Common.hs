module Hex.Stage.Evaluate.Impl.Common where

import ASCII qualified
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

data EvaluationError
  = ValueNotInRange
  | InvalidTokenInBalancedText PT.PrimitiveToken
  deriving stock (Show, Generic)

evalExpandedBalancedTextToText ::
  (MonadError e m, AsType EvaluationError e) =>
  ST.ExpandedBalancedText ->
  m Text
evalExpandedBalancedTextToText bt = do
  -- For each primitive-token in the expanded-balanced-text.
  msgAsciiChars <- forM bt.expBalancedTextTokens $ \t ->
    -- Get the lex-char-cat from the token, if it is the correct token type.
    case preview PT.primTokCharCat t of
      -- If it is the wrong type, throw an error.
      Nothing -> throwError $ injectTyped $ InvalidTokenInBalancedText t
      -- Otherwise, convert the char-code to its equivalent ASCII-character.
      Just lexCharCat ->
        pure $ Code.codeAsAsciiChar $ lexCharCat.lexCCChar
  -- Build a text from the list of ASCII-characters.
  pure $ ASCII.charListToText $ toList msgAsciiChars
