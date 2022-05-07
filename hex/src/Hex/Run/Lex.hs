module Hex.Run.Lex where

import Formatting qualified as F
import Hex.Run.App (App)
import Hex.Stage.Lex.Interface (MonadLexTokenSource (..))
import Hex.Stage.Lex.Interface.Extract
import Hexlude

lexAll :: App [LexToken]
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
