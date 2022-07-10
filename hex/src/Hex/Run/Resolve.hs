module Hex.Run.Resolve where

import Formatting qualified as F
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hex.Run.App (App)
import Hex.Stage.Resolve.Interface
import Hexlude

-- Resolution mode will remain constant over the operation.
resolveAll :: App [(LT.LexToken, Either ResolutionError RT.ResolvedToken)]
resolveAll = go
  where
    go =
      getMayResolvedToken >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtResolveResult :: Fmt [(LT.LexToken, Either ResolutionError RT.ResolvedToken)]
fmtResolveResult = F.intercalated "\n\n" fmtOneResult
  where
    fmtOneResult =
      F.accessed fst LT.fmtLexToken
        <> F.fconst "\n"
        <> F.reindented
          4
          (F.fconst "--r--> " <> F.accessed snd fmtErrOrRT)

    fmtErrOrRT = F.eithered fmtResolutionError RT.fmtResolvedToken
