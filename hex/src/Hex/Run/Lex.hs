module Hex.Run.Lex where

import Formatting qualified as F
import Hex.Common.HexInput.Interface
import Hex.Common.Token.Lexed qualified as LT
import Hexlude

lexAll :: HexInput :> es => Eff es [LT.LexToken]
lexAll = go
  where
    go =
      getNextLexToken >>= \case
        Nothing ->
          pure []
        Just tok -> do
          v <- go
          pure $ tok : v

fmtLexResult :: Fmt [LT.LexToken]
fmtLexResult = F.unlined LT.fmtLexToken
