module Hex.Stage.Parse.Impl.Parsers.BalancedText where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as T
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..), getExpandedLexToken)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hexlude
import qualified Hex.Common.HexState.Interface.TokenList as HSt.TL

parseGeneralText :: MonadPrimTokenParse m => m a -> m a
parseGeneralText parseBalancedText = do
  skipFiller
  parseBalancedText

parseExpandedGeneralText :: MonadPrimTokenParse m => BalancedTextContext -> m T.ExpandedBalancedText
parseExpandedGeneralText ctx = parseGeneralText (parseExpandedBalancedText ctx)

parseInhibitedGeneralText :: MonadPrimTokenParse m => BalancedTextContext -> m T.InhibitedBalancedText
parseInhibitedGeneralText ctx = parseGeneralText (parseInhibitedBalancedText ctx)

data BalancedTextContext = AlreadySeenBeginGroup | ExpectingBeginGroup

skipBeginGroupIfNeeded :: MonadPrimTokenParse m => BalancedTextContext -> m ()
skipBeginGroupIfNeeded = \case
  AlreadySeenBeginGroup -> pure ()
  ExpectingBeginGroup -> skipSatisfied $ primTokenHasCategory Code.BeginGroup

parseExpandedBalancedText :: MonadPrimTokenParse m => BalancedTextContext -> m T.ExpandedBalancedText
parseExpandedBalancedText ctx = do
  skipBeginGroupIfNeeded ctx
  T.ExpandedBalancedText . HSt.TL.BalancedText . fst <$> parseNestedExprExpanded
  where
    parseNestedExprExpanded = parseNestedExpr $ \_depth -> do
      lt <- getExpandedLexToken
      -- Check for a begin- or end-group token. Anything else leaves the
      -- expression depth unchanged.
      pure (lt, lexTokenToGroupDepthChange lt)

parseInhibitedBalancedText :: MonadPrimTokenParse m => BalancedTextContext -> m T.InhibitedBalancedText
parseInhibitedBalancedText ctx = do
  skipBeginGroupIfNeeded ctx
  T.InhibitedBalancedText . HSt.TL.BalancedText . fst <$> parseNestedExprInhibited
  where
    -- Note that we get lex-tokens, so we parse without resolving.
    parseNestedExprInhibited = parseNestedExpr $ \_depth -> do
      lt <- getUnexpandedToken
      pure (lt, lexTokenToGroupDepthChange lt)

lexTokenToGroupDepthChange :: Lex.LexToken -> Ordering
lexTokenToGroupDepthChange t
  | Par.lexTokenHasCategory Code.BeginGroup t = GT
  | Par.lexTokenHasCategory Code.EndGroup t = LT
  | otherwise = EQ

-- Parse a nested expression. The function assumes we have already seen the
-- opening 'begin-group' token.
parseNestedExpr :: Monad m => (Int -> m (a, Ordering)) -> m (Seq a, a)
parseNestedExpr parseNext = go mempty (1 :: Int)
  where
    go acc depth = do
      (a, depthChange) <- parseNext depth
      let newDepth = case depthChange of
            GT -> depth + 1
            LT -> depth - 1
            EQ -> depth
      case newDepth of
        0 -> pure (acc, a)
        _ -> go (acc |> a) newDepth
