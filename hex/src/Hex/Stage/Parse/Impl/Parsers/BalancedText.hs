module Hex.Stage.Parse.Impl.Parsers.BalancedText where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Common.Token.Lexed qualified as LT
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hexlude

parseGeneralText :: MonadPrimTokenParse m => m a -> m a
parseGeneralText parseBalancedText = do
  skipFillerExpanding
  parseBalancedText

parseExpandedGeneralText :: MonadPrimTokenParse m => BalancedTextContext -> m HSt.TL.ExpandedBalancedText
parseExpandedGeneralText ctx = parseGeneralText (parseExpandedBalancedText ctx)

parseInhibitedGeneralText :: MonadPrimTokenParse m => BalancedTextContext -> m HSt.TL.InhibitedBalancedText
parseInhibitedGeneralText ctx = parseGeneralText (parseInhibitedBalancedText ctx)

data BalancedTextContext = AlreadySeenBeginGroup | ExpectingBeginGroup

skipBeginGroupIfNeeded :: MonadPrimTokenParse m => BalancedTextContext -> m ()
skipBeginGroupIfNeeded = \case
  AlreadySeenBeginGroup -> pure ()
  ExpectingBeginGroup -> skipSatisfied Par.satisfyLexThenExpanding $ lexTokenHasCategory Code.BeginGroup

parseExpandedBalancedText :: MonadPrimTokenParse m => BalancedTextContext -> m HSt.TL.ExpandedBalancedText
parseExpandedBalancedText ctx = do
  skipBeginGroupIfNeeded ctx
  HSt.TL.ExpandedBalancedText . HSt.TL.BalancedText . fst <$> parseNestedExprExpanded
  where
    parseNestedExprExpanded = parseNestedExpr $ \_depth -> do
      lt <- anyToken Par.satisfyLexThenExpanding
      -- Check for a begin- or end-group token. Anything else leaves the
      -- expression depth unchanged.
      pure (lt, lexTokenToGroupDepthChange lt)

parseInhibitedBalancedText :: MonadPrimTokenParse m => BalancedTextContext -> m HSt.TL.InhibitedBalancedText
parseInhibitedBalancedText ctx = do
  skipBeginGroupIfNeeded ctx
  HSt.TL.InhibitedBalancedText . HSt.TL.BalancedText . fst <$> parseNestedExprInhibited
  where
    -- Note that we get lex-tokens, so we parse without resolving.
    parseNestedExprInhibited = parseNestedExpr $ \_depth -> do
      lt <- anyToken Par.satisfyThenInhibited
      pure (lt, lexTokenToGroupDepthChange lt)

lexTokenToGroupDepthChange :: LT.LexToken -> Ordering
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
