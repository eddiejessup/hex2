module Hex.Run.Lex where

import Formatting qualified as F
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import Hex.Stage.Lex.Interface.Extract
import Hexlude

lexAll :: MonadLexTokenSource m => m [LexToken]
lexAll = go
  where
    go =
      getLexToken >>= \case
        Nothing ->
          pure []
        Just tok -> do
          v <- go
          pure $ tok : v

fmtLexResult :: Fmt [LexToken]
fmtLexResult = F.unlined fmtLexToken
