module Hex.Run.Resolve where

import Formatting qualified as F
import Hex.Common.HexState.Impl ()
import Hex.Common.HexState.Interface.Resolve (ResolvedToken, fmtResolvedToken)
import Hex.Run.App (App)
import Hex.Stage.Categorise.Impl ()
import Hex.Stage.Lex.Impl ()
import Hex.Stage.Lex.Interface.Extract (fmtLexToken)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Resolve.Impl ()
import Hex.Stage.Resolve.Interface
import Hexlude

-- Resolution mode will remain constant over the operation.
resolveAll :: ResolutionMode -> App [(Lex.LexToken, Either ResolutionError ResolvedToken)]
resolveAll mode = go
  where
    go =
      getMayResolvedToken mode >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtResolveResult :: Fmt [(Lex.LexToken, Either ResolutionError ResolvedToken)] r
fmtResolveResult = F.intercalated "\n\n" fmtOneResult
  where
    fmtOneResult =
      F.accessed fst fmtLexToken
        <> F.fconst "\n"
        <> F.indented
          4
          (F.fconst "--r--> " <> F.accessed snd fmtErrOrRT)

    fmtErrOrRT = F.eithered fmtResolutionError fmtResolvedToken
