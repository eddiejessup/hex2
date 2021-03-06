{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Parse.MonadTokenSource.Impls.CharSource where

import Data.Generics.Sum (AsType, injectTyped)
import Protolude hiding ((%))

import Hex.Lex.Types qualified as H.Lex
import Hex.Lex.Impl qualified as H.Lex
import Hex.Symbol.Resolve as H.Sym.Res
import Hex.Parse.MonadTokenSource.Interface
import Data.Generics.Product qualified as G.P
import Optics.State qualified as O
import Optics.Core ((%), (.~))
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Hex.MonadHexState.Interface qualified as H.St
import Hex.Categorise.Types qualified as H.Cat
import Hex.Parse.CharSource qualified as H.Par.ChrSrc

instance
  ( Monad m,
    MonadState st m,
    G.P.HasType H.Par.ChrSrc.CharSource st,
    MonadError e m,
    AsType H.Lex.LexError e,
    H.St.MonadHexState m
  ) =>
  MonadTokenSource m
  where
  getLexToken = do
    lexState <- O.use (G.P.typed @H.Par.ChrSrc.CharSource % G.P.typed @H.Lex.LexState)
    chars <- O.use (G.P.typed @H.Par.ChrSrc.CharSource % G.P.typed @ByteString)
    runExceptT @H.Lex.LexFailure (H.Lex.extractToken lexState chars) >>= \case
      Left (H.Lex.LexEndOfInputFailure H.Cat.EndOfInput) ->
        pure Nothing
      Left (H.Lex.LexErrorFailure e) ->
        throwError $ injectTyped e
      Right (lt, newLexState, newChars) -> do
        O.modifying' (G.P.typed @H.Par.ChrSrc.CharSource) $
          \src -> src & G.P.field @"sourceChars" .~ newChars & G.P.field @"sourceLexState" .~ newLexState
        pure $ Just lt

  resolveLexToken resMode lt = do
    H.Sym.Res.resolveToken resMode lt <&> \case
      Nothing -> Left ResolutionError
      Just rt -> Right rt

  -- Insert in reverse order, so, we insert the "r" of "relax" last, so we pop "r" next.
  insertLexTokensToSource lts = forM_ (Seq.reverse lts) insertLexTokenToSource

  insertLexTokenToSource lt = O.modifying' (G.P.typed @H.Par.ChrSrc.CharSource % G.P.field @"sourceLexTokens") (:|> lt)

  getSource = O.use (G.P.typed @H.Par.ChrSrc.CharSource)

  putSource = O.assign' (G.P.typed @H.Par.ChrSrc.CharSource)

runTokSrc :: H.Par.ChrSrc.CharSource -> StateT H.Par.ChrSrc.CharSource (ExceptT (Identity H.Lex.LexError) Identity) a -> Either (Identity H.Lex.LexError) (a, H.Par.ChrSrc.CharSource)
runTokSrc ts st = runIdentity (runExceptT (runStateT st ts))
