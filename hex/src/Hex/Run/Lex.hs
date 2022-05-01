module Hex.Run.Lex where

import Hexlude
import Hex.Run.App (App)
import Hex.Stage.Lex.Interface.Extract
import Hex.Stage.Lex.Interface (MonadLexTokenSource(..))
import Hex.Common.HexState.Impl ()
import Hex.Stage.Lex.Impl ()
import qualified Formatting as F

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

fmtLexResult :: Fmt [LexToken] r
fmtLexResult = F.unlined fmtLexToken
