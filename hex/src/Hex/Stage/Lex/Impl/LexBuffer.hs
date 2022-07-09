module Hex.Stage.Lex.Impl.LexBuffer where
import Hexlude (notImplemented)

-- import Data.Sequence qualified as Seq
-- import Formatting qualified as F
-- import Hex.Capability.Log.Interface qualified as Log
-- import Hex.Common.HexState.Interface qualified as HSt
-- import Hex.Stage.Categorise.Interface qualified as Cat
-- import Hex.Stage.Lex.Impl.Extract qualified as Lex
-- import Hex.Stage.Lex.Interface qualified as Lex
-- import Hexlude

extractLexToken :: a
extractLexToken = notImplemented "extractLexToken"

-- extractLexToken ::
--   forall e m st.
--   ( MonadState st m,
--     HasType LexBuffer st,
--     MonadError e m,
--     AsType Lex.LexError e,
--     Cat.MonadCharCatSource m,
--     HSt.MonadHexState m,
--     Log.MonadHexLog m
--   ) =>
--   m (Maybe LT.LexToken)
-- extractLexToken = do
--   -- Get any lex-tokens in the char-source.
--   -- (This is like a short-term buffer.)
--   lexTokens <- use (typed @LexBuffer % typed @(Seq LT.LexToken))
--   case lexTokens of
--     -- If there is at least one lex-token, put the rest back, and return the first one.
--     lt :<| ltRest -> do
--       assign' (typed @LexBuffer % typed @(Seq LT.LexToken)) ltRest
--       Log.log $ "Fetched lex-token from buffer: " <> F.sformat Lex.fmtLexToken lt
--       pure $ Just lt
--     -- If there is no lex-token...
--     Empty -> do
--       -- Get the lex-state and chars, and extract a token from them.
--       LineState <- use (typed @LexBuffer % typed @Lex.LineState)
--       mayLineNr <- use (typed @LexBuffer % typed @Cat.LoadedCharSource % to Cat.charSourceLineNr)
--       runExceptT @e (Lex.extractToken LineState) >>= \case
--         Left e ->
--           throwError e
--         Right Nothing ->
--           pure Nothing
--         Right (Just (lt, newLineState)) -> do
--           assign' (typed @LexBuffer % #bufferLineState) newLineState
--           let logFmtString = Cat.fmtMayLineNr |%| ": Fetched lex-token from source: " |%| Lex.fmtLexToken
--           Log.log $
--             if LineState == newLineState
--               then F.sformat logFmtString mayLineNr lt
--               else F.sformat (logFmtString |%| ", " |%| F.shown |%| " -> " |%| F.shown) mayLineNr lt LineState newLineState
--           pure $ Just lt

-- -- Insert in reverse order, so, we insert the "r" of "relax" last, so we pop "r" next.
-- insertLexTokensToSource :: (HasType LexBuffer s, MonadState s m) => Seq LT.LexToken -> m ()
-- insertLexTokensToSource lts = forM_ (Seq.reverse lts) insertLexTokenToSource

-- insertLexTokenToSource :: (HasType LexBuffer s, MonadState s m) => LT.LexToken -> m ()
-- insertLexTokenToSource lt = modifying' (typed @LexBuffer % #bufferLexTokens) (lt :<|)
