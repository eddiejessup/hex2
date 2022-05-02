{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Resolve.Impl where

import Hex.Common.HexState.Interface qualified as H.St
import Hexlude
import qualified Hex.Common.HexState.Interface as HSt
import Hex.Stage.Resolve.Interface (MonadResolve (..), ResolutionError (..))
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Common.HexState.Interface.Resolve (ResolvedToken (..), ControlSymbol (..))
import qualified Hex.Common.Codes as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken(..))

instance
  ( Monad m,
    HSt.MonadHexState m
  ) =>
  MonadResolve m
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
