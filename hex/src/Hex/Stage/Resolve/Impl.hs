{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Resolve.Impl where

import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Common.HexState.Interface qualified as H.St
import Hexlude
import qualified Hex.Common.HexState.Interface as HSt
import Hex.Stage.Resolve.Interface (MonadResolvedTokenSource (..), ResolutionMode (..), ResolutionError (..))
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Common.HexState.Interface.Resolve (ResolvedToken (..), ControlSymbol (..))
import qualified Hex.Common.Codes as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken(..))

instance
  ( Monad m,
    HSt.MonadHexState m,
    Lex.MonadLexTokenSource m
  ) =>
  MonadResolvedTokenSource m
  where
  resolveLexToken resMode lt = do
    resolveToken resMode lt <&> \case
      Nothing -> Left $ ResolutionError lt
      Just rt -> Right rt

resolveToken ::
  HSt.MonadHexState m =>
  ResolutionMode ->
  Lex.LexToken ->
  m (Maybe ResolvedToken)
resolveToken NotResolving t = pure $ Just $ PrimitiveToken $ UnresolvedTok t
resolveToken Resolving t = case t of
  Lex.ControlSequenceLexToken cs -> do
    H.St.resolveSymbol $ ControlSequenceSymbol cs
  Lex.CharCatLexToken (Lex.LexCharCat c Code.Active) ->
    H.St.resolveSymbol $ ActiveCharacterSymbol c
  _ ->
    pure $ Just $ PrimitiveToken $ UnresolvedTok t
