module Hex.Stage.Parse.Impl.Parsers.BalancedText where

import Hex.Common.Codes qualified as H.C
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as T
import Hex.Common.Parse (MonadPrimTokenParse (..))
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hexlude

parseGeneralText :: MonadPrimTokenParse m => m a -> m a
parseGeneralText parseBalancedText = do
  skipFiller
  parseBalancedText

parseExpandedGeneralText :: MonadPrimTokenParse m => m T.ExpandedBalancedText
parseExpandedGeneralText = parseGeneralText parseExpandedBalancedText

parseInhibitedGeneralText :: MonadPrimTokenParse m => m T.InhibitedBalancedText
parseInhibitedGeneralText = parseGeneralText parseInhibitedBalancedText

parseExpandedBalancedText :: MonadPrimTokenParse m => m T.ExpandedBalancedText
parseExpandedBalancedText = do
  skipSatisfied $ primTokHasCategory H.C.BeginGroup
  T.ExpandedBalancedText <$> parseNestedExprExpanded
  where
    parseNestedExprExpanded = parseNestedExpr $ do
      pt <- getAnyPrimitiveToken
      let tokenCategory = pt ^? PT.primTokCharCat % typed @H.C.CoreCatCode
      pure (pt, tokenCategory)

parseInhibitedBalancedText :: MonadPrimTokenParse m => m T.InhibitedBalancedText
parseInhibitedBalancedText = do
  skipSatisfied $ primTokHasCategory H.C.BeginGroup
  T.InhibitedBalancedText <$> parseNestedExprInhibited
  where
    -- Note that we get lex-tokens, so we parse without resolving.
    parseNestedExprInhibited = parseNestedExpr $ do
      lt <- getAnyLexToken
      let tokenCategory = lt ^? Lex.lexTokCategory
      pure (lt, tokenCategory)

parseNestedExpr :: MonadPrimTokenParse m => m (a, Maybe H.C.CoreCatCode) -> m (Seq a)
parseNestedExpr parseNext = go mempty (1 :: Int)
  where
    go acc depth = case depth of
      0 -> pure acc
      _ -> do
        (a, mayCat) <- parseNext
        let newDepth = case mayCat of
              Just H.C.BeginGroup -> depth + 1
              Just H.C.EndGroup -> depth - 1
              _ -> depth
        case newDepth of
          -- If we just saw an end-group, we want to finish now, to avoid adding
          -- the end-group token itself.
          0 -> pure acc
          _ -> go (acc |> a) newDepth
