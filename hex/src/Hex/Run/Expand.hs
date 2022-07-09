module Hex.Run.Expand where

import Formatting qualified as F
import Hex.Common.Token.Resolved.Primitive
import Hex.Run.App (App)
import Hex.Stage.Expand.Interface (MonadPrimTokenSource (..))
import Hex.Stage.Lex.Interface (fmtLexToken)
import Hex.Stage.Lex.Interface qualified as Lex
import Hexlude

expandAll :: App [(LT.LexToken, PrimitiveToken)]
expandAll = go
  where
    go =
      getPrimitiveToken >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtExpandResult :: Fmt [(LT.LexToken, PrimitiveToken)]
fmtExpandResult = F.intercalated "\n\n" fmtOneResult
  where
    fmtOneResult =
      F.accessed fst fmtLexToken <> F.fconst "\n"
        <> F.reindented
          4
          (F.fconst "--e--> " <> F.accessed snd fmtPrimitiveToken <> F.fconst "\n")
