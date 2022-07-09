{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Resolve.Impl where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve (ControlSymbol (..))
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hex.Common.Token.Resolved.Primitive (PrimitiveToken (..))
import Hex.Stage.Resolve.Interface (MonadResolve (..), ResolutionError (..))
import Hexlude

newtype MonadResolveT m a = MonadResolveT {unMonadResolveT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadError e,
      HSt.MonadHexState,
      MonadHexLog
    )

instance
  ( Monad m,
    HSt.MonadHexState (MonadResolveT m)
  ) =>
  MonadResolve (MonadResolveT m)
  where
  resolveLexToken lt =
    resolveToken lt <&> \case
      Nothing -> Left $ ResolutionError lt
      Just rt -> Right rt

resolveToken ::
  HSt.MonadHexState m =>
  LT.LexToken ->
  m (Maybe RT.ResolvedToken)
resolveToken = \case
  LT.ControlSequenceLexToken cs -> do
    HSt.resolveSymbol $ ControlSequenceSymbol cs
  LT.CharCatLexToken (LT.LexCharCat c Code.Active) ->
    HSt.resolveSymbol $ ActiveCharacterSymbol c
  t ->
    pure $ Just $ RT.PrimitiveToken $ UnresolvedTok t
