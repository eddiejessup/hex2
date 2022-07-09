module Hex.Run.Resolve where

import Formatting qualified as F
import Hex.Common.HexState.Interface.Resolve (ResolvedToken, fmtResolvedToken)
import Hex.Run.App (App)
import Hex.Stage.Lex.Interface (fmtLexToken)
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Resolve.Interface
import Hexlude

-- Resolution mode will remain constant over the operation.
resolveAll :: App [(LT.LexToken, Either ResolutionError ResolvedToken)]
resolveAll = go
  where
    go =
      getMayResolvedToken >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtResolveResult :: Fmt [(LT.LexToken, Either ResolutionError ResolvedToken)]
fmtResolveResult = F.intercalated "\n\n" fmtOneResult
  where
    fmtOneResult =
      F.accessed fst fmtLexToken
        <> F.fconst "\n"
        <> F.reindented
          4
          (F.fconst "--r--> " <> F.accessed snd fmtErrOrRT)

    fmtErrOrRT = F.eithered fmtResolutionError fmtResolvedToken
