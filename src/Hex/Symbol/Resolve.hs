module Hex.Symbol.Resolve where

import Data.HashMap.Strict qualified as HMap
import Hex.Categorise.Types qualified as H.Cat
import Hex.Codes qualified as Code
import Hex.Lex.Impl qualified as H.Lex
import Hex.Lex.Types qualified as H.Lex
import Hex.MonadHexState.Interface qualified as H.St
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hex.Symbol.Types qualified as H.Sym.Ty
import Hexlude

data ResolutionMode = Resolving | NotResolving
  deriving stock (Show, Eq)

type CSMap = HMap.HashMap H.Sym.Ty.ControlSymbol H.Sym.Tok.ResolvedToken

resolveToken ::
  H.St.MonadHexState m =>
  ResolutionMode ->
  H.Lex.LexToken ->
  m (Maybe H.Sym.Tok.ResolvedToken)
resolveToken NotResolving t = pure $ Just $ H.Sym.Tok.PrimitiveToken $ H.Sym.Tok.UnresolvedTok t
resolveToken Resolving t = case t of
  H.Lex.ControlSequenceLexToken cs -> do
    H.St.resolveSymbol $ H.Sym.Ty.ControlSequenceSymbol cs
  H.Lex.CharCatLexToken (H.Lex.LexCharCat c Code.Active) ->
    H.St.resolveSymbol $ H.Sym.Ty.ActiveCharacterSymbol c
  _ ->
    pure $ Just $ H.Sym.Tok.PrimitiveToken $ H.Sym.Tok.UnresolvedTok t

-- Helper to resolve a whole string at once.
codesToResolvedTokens ::
  H.St.MonadHexState m =>
  ResolutionMode ->
  ByteString ->
  ExceptT H.Lex.LexError m [(H.Lex.LexToken, Maybe H.Sym.Tok.ResolvedToken)]
codesToResolvedTokens resMode = go H.Lex.LineBegin
  where
    go lexState xs =
      runExceptT @H.Lex.LexFailure (H.Lex.extractToken lexState xs) >>= \case
        Left (H.Lex.LexEndOfInputFailure H.Cat.EndOfInput) ->
          pure []
        Left (H.Lex.LexErrorFailure le) ->
          throwError le
        Right (tok, lexState1, xs1) -> do
          rt <- resolveToken resMode tok
          v <- go lexState1 xs1
          pure $ (tok, rt) : v
