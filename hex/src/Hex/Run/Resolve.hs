module Hex.Run.Resolve where

import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Stage.Resolve.Interface
import Hex.Run.App (App)
import Hex.Common.HexState.Interface.Resolve (ResolvedToken)
import Hex.Common.HexState.Impl ()
import Hex.Stage.Categorise.Impl ()
import Hex.Stage.Lex.Impl ()
import Hex.Stage.Resolve.Impl ()
import qualified Formatting as F
import Hex.Stage.Lex.Interface.Extract (fmtLexToken)

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
fmtResolveResult = F.unlined fmtOneResult
  where
    fmtOneResult = F.accessed fst fmtLexToken <> F.fconst " --> " <> F.accessed snd fmtErrOrRT

    fmtErrOrRT = F.eithered F.shown F.shown
