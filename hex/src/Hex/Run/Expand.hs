module Hex.Run.Expand where

import Formatting qualified as F
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken
import Hex.Run.App (App)
import Hex.Stage.Expand.Interface (MonadPrimTokenSource (..))
import Hex.Stage.Lex.Interface.Extract (fmtLexToken)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

expandAll :: App [(Lex.LexToken, PrimitiveToken)]
expandAll = go
  where
    go =
      getPrimitiveToken >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtExpandResult :: Fmt [(Lex.LexToken, PrimitiveToken)]
fmtExpandResult = F.intercalated "\n\n" fmtOneResult
  where
    fmtOneResult =
      F.accessed fst fmtLexToken <> F.fconst "\n"
        <> F.reindented
          4
          (F.fconst "--e--> " <> F.accessed snd fmtPrimitiveToken <> F.fconst "\n")
