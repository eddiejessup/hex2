module Hex.Stage.Lex.Impl.CharSource where

import Hex.Stage.Lex.Interface.CharSource qualified as Lex
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude
import Hex.Stage.Lex.Interface.CharSource (CharSource)
import qualified Hex.Common.HexState.Interface as HSt
import qualified Hex.Stage.Lex.Impl.Extract as Lex
import qualified Data.Sequence as Seq

extractLexToken :: forall e m st. ( Monad m,
    MonadState st m,
    HasType CharSource st,
    MonadError e m,
    AsType Lex.LexError e,
    HSt.MonadHexState m
  ) => m (Maybe Lex.LexToken)
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
      chars <- use (typed @CharSource % typed @ByteString)
      runExceptT @e (Lex.extractToken lexState chars) >>= \case
        Left e ->
          throwError e
        Right Nothing ->
          pure Nothing
        Right (Just (lt, newLexState, newChars)) -> do
          modifying' (typed @CharSource) $
            \src -> src & #sourceChars !~ newChars & #sourceLexState !~ newLexState
          pure $ Just lt

  -- Insert in reverse order, so, we insert the "r" of "relax" last, so we pop "r" next.
insertLexTokensToSource :: (HasType CharSource s, MonadState s m) => Seq Lex.LexToken -> m ()
insertLexTokensToSource lts = forM_ (Seq.reverse lts) insertLexTokenToSource

insertLexTokenToSource :: (HasType CharSource s, MonadState s m) => Lex.LexToken -> m ()
insertLexTokenToSource lt = modifying' (typed @CharSource % #sourceLexTokens) (lt :<|)
