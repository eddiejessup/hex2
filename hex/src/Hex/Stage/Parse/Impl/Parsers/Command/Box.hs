module Hex.Stage.Parse.Impl.Parsers.Command.Box where

import Control.Monad.Combinators qualified as PC
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as Code
import Hex.Common.Token.Resolved.Primitive (PrimitiveToken)
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (PrimTokenSource (..), parseFail)
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Glue qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

headToParseLeadersSpec :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => Axis -> PT.PrimitiveToken -> Eff es AST.LeadersSpec
headToParseLeadersSpec axis = \case
  PT.LeadersTok leaders ->
    AST.LeadersSpec leaders <$> parseBoxOrRule <*> (anyPrim >>= Par.headToParseModedAddGlue axis)
  t ->
    parseFail $ "headToParseLeadersSpec " <> F.sformat PT.fmtPrimitiveToken t

headToParseBox :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => PrimitiveToken -> Eff es AST.Box
headToParseBox = \case
  PT.FetchedBoxTok fetchMode ->
    AST.FetchedRegisterBox fetchMode <$> Par.parseExplicitRegisterLocation
  PT.LastBoxTok ->
    pure AST.LastBox
  PT.SplitVBoxTok -> do
    nr <- Par.parseInt
    skipKeyword PT.Expanding [Code.Chr_ 't', Code.Chr_ 'o']
    AST.VSplitBox nr <$> Par.parseLength
  PT.ExplicitBoxTok boxType -> do
    boxSpec <- parseBoxSpecification
    skipCharCatWithCategory PT.Expanding Code.BeginGroup
    pure $ AST.ExplicitBox boxSpec boxType
  t ->
    parseFail $ "headToParseBox " <> F.sformat PT.fmtPrimitiveToken t

parseBoxSpecification :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => Eff es AST.BoxSpecification
parseBoxSpecification = do
  spec <-
    PC.choice
      [ skipKeyword PT.Expanding [Chr_ 't', Chr_ 'o'] *> (AST.To <$> Par.parseLength),
        skipKeyword PT.Expanding [Chr_ 's', Chr_ 'p', Chr_ 'r', Chr_ 'e', Chr_ 'a', Chr_ 'd'] *> (AST.Spread <$> Par.parseLength),
        pure AST.Natural
      ]
  skipFillerExpanding
  pure spec

parseBoxOrRule :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => Eff es AST.BoxOrRule
parseBoxOrRule = do
  headTok <- anyPrim
  PC.choice
    [ AST.BoxOrRuleBox <$> headToParseBox headTok,
      AST.BoxOrRuleRule Horizontal <$> (headToParseModedRule Horizontal headTok),
      AST.BoxOrRuleRule Vertical <$> (headToParseModedRule Vertical headTok)
    ]

-- \hrule and such.
headToParseModedRule :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => Axis -> PT.PrimitiveToken -> Eff es AST.Rule
headToParseModedRule axis = \case
  PT.ModedCommand tokenAxis PT.RuleTok
    | axis == tokenAxis ->
        AST.Rule <$> go mempty
  t ->
    parseFail $ "headToParseModedRule " <> F.sformat PT.fmtPrimitiveToken t
  where
    go dims = do
      skipOptionalSpaces PT.Expanding
      mayDim <-
        PC.optional $
          PC.choice
            [ (Box.BoxWidth,) <$> parseRuleDimen [Chr_ 'w', Chr_ 'i', Chr_ 'd', Chr_ 't', Chr_ 'h'],
              (Box.BoxHeight,) <$> parseRuleDimen [Chr_ 'h', Chr_ 'e', Chr_ 'i', Chr_ 'g', Chr_ 'h', Chr_ 't'],
              (Box.BoxDepth,) <$> parseRuleDimen [Chr_ 'd', Chr_ 'e', Chr_ 'p', Chr_ 't', Chr_ 'h']
            ]
      case mayDim of
        Just newDim -> go (dims |> newDim)
        Nothing -> pure dims

    parseRuleDimen keyword = do
      skipKeyword PT.Expanding keyword
      Par.parseLength

headToParseFetchedBoxRef :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => Axis -> PT.PrimitiveToken -> Eff es AST.FetchedBoxRef
headToParseFetchedBoxRef tgtAxis = \case
  PT.ModedCommand tokenAxis (PT.UnwrappedFetchedBoxTok fetchMode) | tgtAxis == tokenAxis -> do
    n <- Par.parseInt
    pure $ AST.FetchedBoxRef n fetchMode
  t ->
    parseFail $ "headToParseFetchedBoxRef " <> F.sformat PT.fmtPrimitiveToken t
