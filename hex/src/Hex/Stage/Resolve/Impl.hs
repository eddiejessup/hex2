{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Resolve.Impl where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as H.St
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve (ControlSymbol (..), ResolvedToken (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken (..))
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Resolve.Interface (MonadResolve (..), ResolutionError (..))
import Hexlude

newtype MonadResolveT m a = MonadResolveT {unMonadResolveT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState st, MonadError e, HSt.MonadHexState)

instance
  ( Monad (MonadResolveT m),
    HSt.MonadHexState (MonadResolveT m)
  ) =>
  MonadResolve (MonadResolveT m)
  where
  resolveLexToken lt = do
    resolveToken lt <&> \case
      Nothing -> Left $ ResolutionError lt
      Just rt -> Right rt

resolveToken ::
  HSt.MonadHexState m =>
  Lex.LexToken ->
  m (Maybe ResolvedToken)
resolveToken = \case
  Lex.ControlSequenceLexToken cs -> do
    H.St.resolveSymbol $ ControlSequenceSymbol cs
  Lex.CharCatLexToken (Lex.LexCharCat c Code.Active) ->
    H.St.resolveSymbol $ ActiveCharacterSymbol c
  t ->
    pure $ Just $ PrimitiveToken $ UnresolvedTok t
