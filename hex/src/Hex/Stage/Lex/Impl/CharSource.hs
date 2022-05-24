module Hex.Stage.Lex.Impl.CharSource where

import Data.Sequence qualified as Seq
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Stage.Categorise.Interface qualified as Cat
import Hex.Stage.Lex.Impl.Extract qualified as Lex
import Hex.Stage.Lex.Interface.CharSource (CharSource)
import Hex.Stage.Lex.Interface.CharSource qualified as Lex
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

extractLexToken ::
  forall e m st.
  ( Monad m,
    MonadState st m,
    HasType CharSource st,
    MonadError e m,
    AsType Lex.LexError e,
    Cat.MonadCharCatSource m,
    HSt.MonadHexState m
  ) =>
  m (Maybe Lex.LexToken)
extractLexToken = do
  -- Get any lex-tokens in the char-source.
  -- (This is like a short-term buffer.)
  lexTokens <- use (typed @CharSource % typed @(Seq Lex.LexToken))
  case lexTokens of
    -- If there is at least one lex-token, put the rest back, and return the first one.
    lt :<| ltRest -> do
      assign' (typed @CharSource % typed @(Seq Lex.LexToken)) ltRest
      pure $ Just lt
    -- If there is no lex-token...
    Empty -> do
      -- Get the lex-state and chars, and extract a token from them.
      lexState <- use (typed @CharSource % typed @Lex.LexState)
      runExceptT @e (Lex.extractToken lexState) >>= \case
        Left e ->
          throwError e
        Right Nothing ->
          pure Nothing
        Right (Just (lt, newLexState)) -> do
          modifying' (typed @CharSource) $ #sourceLexState !~ newLexState
          pure $ Just lt

-- Insert in reverse order, so, we insert the "r" of "relax" last, so we pop "r" next.
insertLexTokensToSource :: (HasType CharSource s, MonadState s m) => Seq Lex.LexToken -> m ()
insertLexTokensToSource lts = forM_ (Seq.reverse lts) insertLexTokenToSource

insertLexTokenToSource :: (HasType CharSource s, MonadState s m) => Lex.LexToken -> m ()
insertLexTokenToSource lt = modifying' (typed @CharSource % #sourceLexTokens) (lt :<|)
