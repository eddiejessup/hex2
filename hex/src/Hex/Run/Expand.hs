module Hex.Run.Expand where

import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Run.App (App)
import Hex.Common.HexState.Interface.Resolve (ResolvedToken, fmtResolvedToken)
import Hex.Common.HexState.Impl ()
import Hex.Stage.Categorise.Impl ()
import Hex.Stage.Lex.Impl ()
import Hex.Stage.Resolve.Impl ()
import Hex.Stage.Expand.Impl ()
import qualified Formatting as F
import Hex.Stage.Lex.Interface.Extract (fmtLexToken)
import Hex.Stage.Expand.Interface (MonadPrimTokenSource(..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken

expandAll :: App [(Lex.LexToken, ResolvedToken, PrimitiveToken)]
expandAll = go
  where
    go =
      getPrimitiveToken >>= \case
        Nothing ->
          pure []
        Just r -> do
          v <- go
          pure $ r : v

fmtExpandResult :: Fmt [(Lex.LexToken, ResolvedToken, PrimitiveToken)] r
fmtExpandResult = F.unlined fmtOneResult
  where
    fmtOneResult =
      F.accessed (\(x, _, _) -> x) fmtLexToken <> F.fconst "\n"
      <> F.indented 4
        (F.fconst "-r> " <> F.accessed (\(_, x, _) -> x) fmtResolvedToken <> F.fconst "\n")
      <> F.indented 4
        (F.fconst "-e> " <> F.accessed (\(_, _, x) -> x) fmtPrimitiveToken <> F.fconst "\n")
