module Hex.Stage.Parse.Impl.Parsers.BalancedText where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as T
import Hex.Common.Parse (MonadPrimTokenParse (..))
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hexlude

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
  T.ExpandedBalancedText <$> parseNestedExprExpanded
  where
    parseNestedExprExpanded = parseNestedExpr $ do
      pt <- getAnyPrimitiveToken
      let -- Check for a begin- or end-group token. Anything else leaves the
          -- expression depth unchanged.
          tokenOrdering =
            fromMaybe EQ $ pt ^? PT.primTokLexTok % to lexTokenToGroupDepthChange
      pure (pt, tokenOrdering)

parseInhibitedBalancedText :: MonadPrimTokenParse m => BalancedTextContext -> m T.InhibitedBalancedText
parseInhibitedBalancedText ctx = do
  skipBeginGroupIfNeeded ctx
  T.InhibitedBalancedText <$> parseNestedExprInhibited
  where
    -- Note that we get lex-tokens, so we parse without resolving.
    parseNestedExprInhibited = parseNestedExpr $ do
      lt <- getAnyLexToken
      pure (lt, lexTokenToGroupDepthChange lt)

lexTokenToGroupDepthChange :: Lex.LexToken -> Ordering
lexTokenToGroupDepthChange t
  | Par.lexTokenHasCategory Code.BeginGroup t = GT
  | Par.lexTokenHasCategory Code.EndGroup t = LT
  | otherwise = EQ

-- Parse a nested expression. The function assumes we have already seen the
-- opening 'begin-group' token.
parseNestedExpr :: MonadPrimTokenParse m => m (a, Ordering) -> m (Seq a)
parseNestedExpr parseNext = go mempty (1 :: Int)
  where
    go acc depth = case depth of
      0 -> pure acc
      _ -> do
        (a, mayCat) <- parseNext
        let newDepth = case mayCat of
              GT -> depth + 1
              LT -> depth - 1
              EQ -> depth
        case newDepth of
          -- If we just saw an end-group, we want to finish now, to avoid adding
          -- the end-group token itself.
          0 -> pure acc
          _ -> go (acc |> a) newDepth
