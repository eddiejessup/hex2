module Hex.Stage.Parse.Impl.Parsers.Command.Box where

import Control.Monad.Combinators qualified as PC
import Formatting qualified as F
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as Code
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..), parseFailure)
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Resolved.Primitive (PrimitiveToken)
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Glue qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

headToParseLeadersSpec :: MonadPrimTokenParse m => Q.Axis -> PT.PrimitiveToken -> m AST.LeadersSpec
headToParseLeadersSpec axis = \case
  PT.LeadersTok leaders ->
    AST.LeadersSpec leaders <$> parseBoxOrRule <*> (anyPrim >>= Par.headToParseModedAddGlue axis)
  t ->
    parseFailure $ "headToParseLeadersSpec " <> F.sformat PT.fmtPrimitiveToken t

headToParseBox :: MonadPrimTokenParse m => PrimitiveToken -> m AST.Box
headToParseBox = \case
  PT.FetchedBoxTok fetchMode ->
    AST.FetchedRegisterBox fetchMode <$> Par.parseExplicitRegisterLocation
  PT.LastBoxTok ->
    pure AST.LastBox
  PT.SplitVBoxTok -> do
    nr <- Par.parseInt
    skipKeyword Expanding [Code.Chr_ 't', Code.Chr_ 'o']
    AST.VSplitBox nr <$> Par.parseLength
  PT.ExplicitBoxTok boxType -> do
    boxSpec <- parseBoxSpecification
    skipSatisfied (satisfyCharCatThen Expanding) (charCatHasCategory Code.BeginGroup)
    pure $ AST.ExplicitBox boxSpec boxType
  t ->
    parseFailure $ "headToParseBox " <> F.sformat PT.fmtPrimitiveToken t
  where
    parseBoxSpecification = do
      spec <-
        PC.choice
          [ skipKeyword Expanding [Chr_ 't', Chr_ 'o'] *> (AST.To <$> Par.parseLength),
            skipKeyword Expanding [Chr_ 's', Chr_ 'p', Chr_ 'r', Chr_ 'e', Chr_ 'a', Chr_ 'd'] *> (AST.Spread <$> Par.parseLength),
            pure AST.Natural
          ]
      skipFillerExpanding
      pure spec

parseBoxOrRule :: MonadPrimTokenParse m => m AST.BoxOrRule
parseBoxOrRule =
  PC.choice
    [ AST.BoxOrRuleBox <$> (anyPrim >>= headToParseBox),
      AST.BoxOrRuleRule Q.Horizontal <$> (anyPrim >>= (headToParseModedRule Q.Horizontal)),
      AST.BoxOrRuleRule Q.Vertical <$> (anyPrim >>= (headToParseModedRule Q.Vertical))
    ]

-- \hrule and such.
headToParseModedRule :: MonadPrimTokenParse m => Q.Axis -> PT.PrimitiveToken -> m AST.Rule
headToParseModedRule axis = \case
  PT.ModedCommand tokenAxis PT.RuleTok
    | axis == tokenAxis ->
        AST.Rule <$> go mempty
  t ->
    parseFailure $ "headToParseModedRule " <> F.sformat PT.fmtPrimitiveToken t
  where
    go dims = do
      skipOptionalSpaces Expanding
      mayDim <-
        PC.optional $
          PC.choice
            [ parseRuleDimen [Chr_ 'w', Chr_ 'i', Chr_ 'd', Chr_ 't', Chr_ 'h'] Q.BoxWidth,
              parseRuleDimen [Chr_ 'h', Chr_ 'e', Chr_ 'i', Chr_ 'g', Chr_ 'h', Chr_ 't'] Q.BoxHeight,
              parseRuleDimen [Chr_ 'd', Chr_ 'e', Chr_ 'p', Chr_ 't', Chr_ 'h'] Q.BoxDepth
            ]
      case mayDim of
        Just newDim -> go (dims :|> newDim)
        Nothing -> pure dims

    parseRuleDimen keyword dimType = do
      skipKeyword Expanding keyword
      ln <- Par.parseLength
      pure (dimType, ln)

headToParseFetchedBoxRef :: MonadPrimTokenParse m => Q.Axis -> PT.PrimitiveToken -> m AST.FetchedBoxRef
headToParseFetchedBoxRef tgtAxis = \case
  PT.ModedCommand tokenAxis (PT.UnwrappedFetchedBoxTok fetchMode) | tgtAxis == tokenAxis -> do
    n <- Par.parseInt
    pure $ AST.FetchedBoxRef n fetchMode
  t ->
    parseFailure $ "headToParseFetchedBoxRef " <> F.sformat PT.fmtPrimitiveToken t
