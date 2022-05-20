{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Resolve.Impl where

import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Resolve (ControlSymbol (..), ResolvedToken (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken (..))
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Resolve.Interface (MonadResolve (..), ResolutionError (..))
import Hexlude
import qualified Hex.Capability.Log.Interface as Log

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
  ( Monad (MonadResolveT m),
    HSt.MonadHexState (MonadResolveT m),
    Log.MonadHexLog (MonadResolveT m)
  ) =>
  MonadResolve (MonadResolveT m)
  where
  resolveLexToken lt = Log.log ("resolveLexToken: " <> show lt) >>
    resolveToken lt <&> \case
      Nothing -> Left $ ResolutionError lt
      Just rt -> Right rt

resolveToken ::
  HSt.MonadHexState m =>
  Lex.LexToken ->
  m (Maybe ResolvedToken)
resolveToken = \case
  Lex.ControlSequenceLexToken cs -> do
    HSt.resolveSymbol $ ControlSequenceSymbol cs
  Lex.CharCatLexToken (Lex.LexCharCat c Code.Active) ->
    HSt.resolveSymbol $ ActiveCharacterSymbol c
  t ->
    pure $ Just $ PrimitiveToken $ UnresolvedTok t
