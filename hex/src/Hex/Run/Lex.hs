module Hex.Run.Lex where

import Formatting qualified as F
import Hex.Common.Token.Lexed qualified as LT
import Hex.Stage.Lex.Interface
import Hexlude

lexAll :: MonadLexTokenSource m => m [LT.LexToken]
lexAll = go
  where
    go =
      getLexToken >>= \case
        Nothing ->
          pure []
        Just tok -> do
          v <- go
          pure $ tok : v

fmtLexResult :: Fmt [LT.LexToken]
fmtLexResult = F.unlined LT.fmtLexToken
