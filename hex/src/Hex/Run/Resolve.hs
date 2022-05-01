module Hex.Run.Resolve where

import Hexlude
import qualified Hex.Stage.Lex.Impl.Extract as Lex
import qualified Hex.Common.HexState.Interface.Resolve as H.Res
import qualified Hex.Stage.Resolve.Interface as H.Stage.Res
import qualified Hex.Common.HexState.Interface as HSt
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Stage.Resolve.Impl (resolveToken)

-- Helper to resolve a whole string at once.
codesToResolvedTokens ::
  HSt.MonadHexState m =>
  H.Stage.Res.ResolutionMode ->
  ByteString ->
  ExceptT Lex.LexError m [(Lex.LexToken, Maybe H.Res.ResolvedToken)]
codesToResolvedTokens resMode xs0 = withExceptT runIdentity $ go Lex.LineBegin xs0
  where
    go lexState xs =
      Lex.extractToken @(Identity Lex.LexError) lexState xs >>= \case
        Nothing ->
          pure []
        Just (tok, lexState1, xs1) -> do
          rt <- lift $ resolveToken resMode tok
          v <- go lexState1 xs1
          pure $ (tok, rt) : v
