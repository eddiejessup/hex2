{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Lex.Impl where

import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Lex.Impl.CharSource qualified as Impl
import Hex.Stage.Lex.Interface
import Hex.Stage.Lex.Interface.CharSource
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

instance
  ( Monad m,
    MonadState st m,
    HasType CharSource st,
    MonadError e m,
    AsType Lex.LexError e,
    HSt.MonadHexState m
  ) =>
  MonadLexTokenSource m
  where
  getLexToken = Impl.extractLexToken

  insertLexTokensToSource = Impl.insertLexTokensToSource

  insertLexTokenToSource = Impl.insertLexTokenToSource

  getSource = use (typed @CharSource)

  putSource = assign' (typed @CharSource)
