module Hex.Stage.Parse.Impl.Parsers.BalancedText where

import Hex.Common.Codes qualified as H.C
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as T
import Hex.Stage.Expand.Impl.Parsing (MonadPrimTokenParse (..))
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
  T.ExpandedBalancedText <$> parseNestedExpr (getAnyPrimitiveToken <&> \pt -> (pt, pt ^? primTokCharCat % typed @H.C.CoreCatCode))

parseInhibitedBalancedText :: MonadPrimTokenParse m => m T.InhibitedBalancedText
parseInhibitedBalancedText = do
  skipSatisfied $ primTokHasCategory H.C.BeginGroup
  T.InhibitedBalancedText <$> withInhibition inhibParseNestedExpr
  where
    inhibParseNestedExpr inhibToken =
      parseNestedExpr $ inhibFetchLexToken inhibToken <&> \lt -> (lt, lt ^? _Typed @Lex.LexCharCat % typed @H.C.CoreCatCode)

parseNestedExpr :: MonadPrimTokenParse m => m (a, Maybe H.C.CoreCatCode) -> m (Seq a)
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
