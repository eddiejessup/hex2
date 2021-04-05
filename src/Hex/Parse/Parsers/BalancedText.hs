{-# LANGUAGE PatternSynonyms #-}

module Hex.Parse.Parsers.BalancedText where

import Hex.Codes qualified as H.C
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators
import Hex.Symbol.Tokens qualified as T
import Hexlude

parseGeneralText :: MonadPrimTokenSource m => m a -> m a
parseGeneralText parseBalancedText = do
  skipFiller
  parseBalancedText

parseExpandedGeneralText :: MonadPrimTokenSource m => m T.ExpandedBalancedText
parseExpandedGeneralText = parseGeneralText parseExpandedBalancedText

parseInhibitedGeneralText :: MonadPrimTokenSource m => m T.InhibitedBalancedText
parseInhibitedGeneralText = parseGeneralText parseInhibitedBalancedText

parseExpandedBalancedText :: MonadPrimTokenSource m => m T.ExpandedBalancedText
parseExpandedBalancedText = do
  skipSatisfied $ primTokHasCategory H.C.BeginGroup
  T.ExpandedBalancedText <$> parseNestedExpr (fetchPT <&> \pt -> (pt, pt ^? primTokCharCat % typed @H.C.CoreCatCode))

parseInhibitedBalancedText :: MonadPrimTokenSource m => m T.InhibitedBalancedText
parseInhibitedBalancedText = do
  skipSatisfied $ primTokHasCategory H.C.BeginGroup
  T.InhibitedBalancedText <$> withInhibition inhibParseNestedExpr
  where
    inhibParseNestedExpr inhibToken = parseNestedExpr $ inhibFetchLexToken inhibToken <&> \lt -> (lt, lt ^? _Typed @H.Lex.LexCharCat % typed @H.C.CoreCatCode)

parseNestedExpr :: MonadPrimTokenSource m => m (a, Maybe H.C.CoreCatCode) -> m (Seq a)
parseNestedExpr parseNext = go mempty (1 :: Int)
  where
    go acc = \case
      0 -> pure acc
      depth -> do
        (a, mayCat) <- parseNext
        go (acc |> a) $ case mayCat of
          Just H.C.BeginGroup -> depth + 1
          Just H.C.EndGroup -> depth - 1
          _ -> depth
