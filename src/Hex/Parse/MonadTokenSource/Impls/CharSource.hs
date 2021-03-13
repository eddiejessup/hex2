{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Parse.MonadTokenSource.Impls.CharSource where

import Data.Sequence qualified as Seq
import Hex.Categorise.Types qualified as H.Cat
import Hex.Lex.Impl qualified as H.Lex
import Hex.Lex.Types qualified as H.Lex
import Hex.MonadHexState.Interface qualified as H.St
import Hex.Parse.CharSource qualified as H.Par.ChrSrc
import Hex.Parse.MonadTokenSource.Interface
import Hex.Symbol.Resolve as H.Sym.Res
import Hexlude

instance
  ( Monad m,
    MonadState st m,
    HasType H.Par.ChrSrc.CharSource st,
    MonadError e m,
    AsType H.Lex.LexError e,
    H.St.MonadHexState m
  ) =>
  MonadTokenSource m
  where
  getLexToken = do
    lexTokens <- use (typed @H.Par.ChrSrc.CharSource % typed @(Seq H.Lex.LexToken))
    case lexTokens of
      lt :<| ltRest -> do
        assign' (typed @H.Par.ChrSrc.CharSource % typed @(Seq H.Lex.LexToken)) ltRest
        pure $ Just lt
      Empty -> do
        lexState <- use (typed @H.Par.ChrSrc.CharSource % typed @H.Lex.LexState)
        chars <- use (typed @H.Par.ChrSrc.CharSource % typed @ByteString)
        runExceptT @H.Lex.LexFailure (H.Lex.extractToken lexState chars) >>= \case
          Left (H.Lex.LexEndOfInputFailure H.Cat.EndOfInput) ->
            pure Nothing
          Left (H.Lex.LexErrorFailure e) ->
            throwError $ injectTyped e
          Right (lt, newLexState, newChars) -> do
            modifying' (typed @H.Par.ChrSrc.CharSource) $
              \src -> src & field @"sourceChars" .~ newChars & field @"sourceLexState" .~ newLexState
            pure $ Just lt

  resolveLexToken resMode lt = do
    H.Sym.Res.resolveToken resMode lt <&> \case
      Nothing -> Left ResolutionError
      Just rt -> Right rt

  -- Insert in reverse order, so, we insert the "r" of "relax" last, so we pop "r" next.
  insertLexTokensToSource lts = forM_ (Seq.reverse lts) insertLexTokenToSource

  insertLexTokenToSource lt = modifying' (typed @H.Par.ChrSrc.CharSource % field @"sourceLexTokens") (lt :<|)

  getSource = use (typed @H.Par.ChrSrc.CharSource)

  putSource = assign' (typed @H.Par.ChrSrc.CharSource)

runTokSrc :: H.Par.ChrSrc.CharSource -> StateT H.Par.ChrSrc.CharSource (ExceptT (Identity H.Lex.LexError) Identity) a -> Either (Identity H.Lex.LexError) (a, H.Par.ChrSrc.CharSource)
runTokSrc ts st = runIdentity (runExceptT (runStateT st ts))
