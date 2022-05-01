module Hex.Stage.Parse.Impl.Parsers.Command.Box where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as H.C
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Glue qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Common.Quantity qualified as H.Q
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hexlude
import Hex.Stage.Expand.Interface (MonadPrimTokenSource)

headToParseLeadersSpec :: MonadPrimTokenSource m => H.Q.Axis -> T.PrimitiveToken -> m AST.LeadersSpec
headToParseLeadersSpec axis = \case
  T.LeadersTok leaders ->
    AST.LeadersSpec leaders <$> parseBoxOrRule <*> parseHeaded (Par.headToParseModedAddGlue axis)
  _ ->
    empty

headToParseBox :: MonadPrimTokenSource m => PrimitiveToken -> m AST.Box
headToParseBox = \case
  T.FetchedBoxTok fetchMode ->
    AST.FetchedRegisterBox fetchMode <$> Par.parseInt
  T.LastBoxTok ->
    pure AST.LastBox
  T.SplitVBoxTok -> do
    nr <- Par.parseInt
    skipKeyword [H.C.Chr_ 't', H.C.Chr_ 'o']
    AST.VSplitBox nr <$> Par.parseLength
  T.ExplicitBoxTok boxType -> do
    boxSpec <- parseBoxSpecification
    skipSatisfied $ primTokHasCategory H.C.BeginGroup
    pure $ AST.ExplicitBox boxSpec boxType
  _ ->
    empty
  where
    parseBoxSpecification = do
      spec <-
        PC.choice
          [ skipKeyword [Chr_ 't', Chr_ 'o'] *> (AST.To <$> Par.parseLength),
            skipKeyword [Chr_ 's', Chr_ 'p', Chr_ 'r', Chr_ 'e', Chr_ 'a', Chr_ 'd'] *> (AST.Spread <$> Par.parseLength),
            pure AST.Natural
          ]
      skipFiller
      pure spec

parseBoxOrRule :: MonadPrimTokenSource m => m AST.BoxOrRule
parseBoxOrRule =
  PC.choice
    [ AST.BoxOrRuleBox <$> parseHeaded headToParseBox,
      AST.BoxOrRuleRule H.Q.Horizontal <$> parseHeaded (headToParseModedRule H.Q.Horizontal),
      AST.BoxOrRuleRule H.Q.Vertical <$> parseHeaded (headToParseModedRule H.Q.Vertical)
    ]

-- \hrule and such.
headToParseModedRule :: MonadPrimTokenSource m => H.Q.Axis -> T.PrimitiveToken -> m AST.Rule
headToParseModedRule axis = \case
  T.ModedCommand tokenAxis T.RuleTok
    | axis == tokenAxis ->
      AST.Rule <$> go mempty
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

headToParseFetchedBoxRef :: MonadPrimTokenSource m => H.Q.Axis -> T.PrimitiveToken -> m AST.FetchedBoxRef
headToParseFetchedBoxRef tgtAxis = \case
  T.ModedCommand tokenAxis (T.UnwrappedFetchedBoxTok fetchMode) | tgtAxis == tokenAxis -> do
    n <- Par.parseInt
    pure $ AST.FetchedBoxRef n fetchMode
  _ ->
    empty
