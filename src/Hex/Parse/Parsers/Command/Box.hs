{-# LANGUAGE PatternSynonyms #-}

module Hex.Parse.Parsers.Command.Box where

import Control.Monad.Combinators qualified as PC
import Hex.Codes (pattern Chr_)
import Hex.Codes qualified as H.C
import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators
import Hex.Parse.Parsers.Quantity.Glue qualified as Par
import Hex.Parse.Parsers.Quantity.Length qualified as Par
import Hex.Parse.Parsers.Quantity.Number qualified as Par
import Hex.Parse.Syntax.Command qualified as H.Par.Syn
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive (PrimitiveToken)
import Hex.Symbol.Token.Primitive qualified as T
import Hexlude

headToParseLeadersSpec :: MonadPrimTokenSource m => H.Q.Axis -> T.PrimitiveToken -> m (H.Syn.LeadersSpec 'H.Syn.Parsed)
headToParseLeadersSpec axis = \case
  T.LeadersTok leaders ->
    H.Syn.LeadersSpec leaders <$> parseBoxOrRule <*> parseHeaded (Par.headToParseModedAddGlue axis)
  _ ->
    empty

headToParseBox :: MonadPrimTokenSource m => PrimitiveToken -> m (H.Syn.Box 'H.Syn.Parsed)
headToParseBox = \case
  T.FetchedBoxTok fetchMode ->
    H.Syn.FetchedRegisterBox fetchMode <$> Par.parseInt
  T.LastBoxTok ->
    pure H.Syn.LastBox
  T.SplitVBoxTok -> do
    nr <- Par.parseInt
    skipKeyword [H.C.Chr_ 't', H.C.Chr_ 'o']
    H.Syn.VSplitBox nr <$> Par.parseLength
  T.ExplicitBoxTok boxType -> do
    boxSpec <- parseBoxSpecification
    skipSatisfied $ primTokHasCategory H.C.BeginGroup
    pure $ H.Syn.ExplicitBox boxSpec boxType
  _ ->
    empty
  where
    parseBoxSpecification = do
      spec <-
        PC.choice
          [ skipKeyword [Chr_ 't', Chr_ 'o'] *> (H.Syn.To <$> Par.parseLength),
            skipKeyword [Chr_ 's', Chr_ 'p', Chr_ 'r', Chr_ 'e', Chr_ 'a', Chr_ 'd'] *> (H.Syn.Spread <$> Par.parseLength),
            pure H.Syn.Natural
          ]
      skipFiller
      pure spec

parseBoxOrRule :: MonadPrimTokenSource m => m (H.Syn.BoxOrRule 'H.Syn.Parsed)
parseBoxOrRule =
  PC.choice
    [ H.Syn.BoxOrRuleBox <$> parseHeaded headToParseBox,
      H.Syn.BoxOrRuleRule H.Q.Horizontal <$> parseHeaded (headToParseModedRule H.Q.Horizontal),
      H.Syn.BoxOrRuleRule H.Q.Vertical <$> parseHeaded (headToParseModedRule H.Q.Vertical)
    ]

-- \hrule and such.
headToParseModedRule :: MonadPrimTokenSource m => H.Q.Axis -> T.PrimitiveToken -> m (H.Syn.HexPassRule 'H.Syn.Parsed)
headToParseModedRule axis = \case
  T.ModedCommand tokenAxis T.RuleTok
    | axis == tokenAxis ->
      H.Par.Syn.Rule <$> go mempty
  _ ->
    empty
  where
    go dims = do
      skipOptionalSpaces
      mayDim <-
        PC.optional $
          PC.choice
            [ parseRuleDimen [Chr_ 'w', Chr_ 'i', Chr_ 'd', Chr_ 't', Chr_ 'h'] H.Q.BoxWidth,
              parseRuleDimen [Chr_ 'h', Chr_ 'e', Chr_ 'i', Chr_ 'g', Chr_ 'h', Chr_ 't'] H.Q.BoxHeight,
              parseRuleDimen [Chr_ 'd', Chr_ 'e', Chr_ 'p', Chr_ 't', Chr_ 'h'] H.Q.BoxDepth
            ]
      case mayDim of
        Just newDim -> go (dims :|> newDim)
        Nothing -> pure dims

    parseRuleDimen keyword dimType = do
      skipKeyword keyword
      ln <- Par.parseLength
      pure (dimType, ln)

headToParseFetchedBoxRef :: MonadPrimTokenSource m => H.Q.Axis -> T.PrimitiveToken -> m (H.Syn.FetchedBoxRef 'H.Syn.Parsed)
headToParseFetchedBoxRef tgtAxis = \case
  T.ModedCommand tokenAxis (T.UnwrappedFetchedBoxTok fetchMode) | tgtAxis == tokenAxis -> do
    n <- Par.parseInt
    pure $ H.Syn.FetchedBoxRef n fetchMode
  _ ->
    empty
