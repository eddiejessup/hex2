module Hex.Run.Expand where

import Formatting qualified as F
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive
import Hex.Stage.Expand.Interface (PrimTokenSource (..), getPrimitiveToken)
import Hexlude

expandAll :: PrimTokenSource :> es => Eff es [(LT.LexToken, PrimitiveToken)]
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
      F.accessed fst LT.fmtLexToken
        <> F.fconst "\n"
        <> F.reindented
          4
          (F.fconst "--e--> " <> F.accessed snd fmtPrimitiveToken <> F.fconst "\n")
