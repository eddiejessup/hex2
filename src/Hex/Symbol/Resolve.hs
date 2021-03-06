module Hex.Symbol.Resolve where

import Protolude

import Data.HashMap.Strict qualified as HMap
import Data.ByteString qualified as BS

import Hex.Codes qualified as Code
import Hex.Lex.Types qualified as H.Lex
import Hex.Lex.Impl qualified as H.Lex
import Hex.Symbol.Tokens
import Hex.Categorise.Types qualified as H.Cat
import Hex.MonadHexState.Interface qualified as H.St

data ResolutionMode = Resolving | NotResolving
    deriving stock ( Show, Eq )

type CSMap = HMap.HashMap H.Lex.LexSymbol ResolvedToken

resolveToken :: H.St.MonadHexState m
             => ResolutionMode
             -> H.Lex.LexToken
             -> m (Maybe ResolvedToken)
resolveToken NotResolving t = pure $ Just $ PrimitiveToken $ UnresolvedTok t
resolveToken Resolving t = case t of
    H.Lex.ControlSequenceLexToken cs -> do
      H.St.resolveSymbol $ H.Lex.ControlSequenceSymbol cs
    H.Lex.CharCatLexToken (H.Lex.LexCharCat c Code.Active) ->
      H.St.resolveSymbol $ H.Lex.ActiveCharacterSymbol c
    _ ->
      pure $ Just $ PrimitiveToken $ UnresolvedTok t

-- Helper to resolve a whole string at once.
codesToResolvedTokens
  :: H.St.MonadHexState m
  => ResolutionMode
  -> BS.ByteString
  -> ExceptT H.Lex.LexError m [(H.Lex.LexToken, Maybe ResolvedToken)]
codesToResolvedTokens resMode = go H.Lex.LineBegin
  where
    go lexState xs = runExceptT @H.Lex.LexFailure (H.Lex.extractToken lexState xs) >>= \case
      Left (H.Lex.LexEndOfInputFailure H.Cat.EndOfInput )->
        pure []
      Left (H.Lex.LexErrorFailure le) ->
        throwError le
      Right (tok, lexState1, xs1) -> do
        rt <- resolveToken resMode tok
        v <- go lexState1 xs1
        pure $ (tok, rt) : v
