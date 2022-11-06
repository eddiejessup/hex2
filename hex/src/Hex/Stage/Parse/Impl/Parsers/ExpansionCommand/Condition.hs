module Hex.Stage.Parse.Impl.Parsers.ExpansionCommand.Condition where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (PrimTokenSource (..))
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.ExpansionCommand qualified as AST
import Hexlude

parseRelationExpanding :: (PrimTokenSource :> es) => Eff es Ordering
parseRelationExpanding = Par.satisfyCharCatThen PT.Expanding $ \cc ->
  if
      | Par.isOnly (Par.charCatChar Code.Other) (Code.Chr_ '<') cc -> Just LT
      | Par.isOnly (Par.charCatChar Code.Other) (Code.Chr_ '>') cc -> Just GT
      | Par.isOnly (Par.charCatChar Code.Other) (Code.Chr_ '=') cc -> Just EQ
      | otherwise -> Nothing

parseConditionHead :: (PrimTokenSource :> es, NonDet :> es, Log.HexLog :> es) => ST.ConditionHeadTok -> Eff es AST.ConditionHead
parseConditionHead = \case
  ST.IfIntPairTestTok ->
    AST.IfConditionHead <$> (AST.IfIntPairTest <$> Par.parseInt <*> parseRelationExpanding <*> Par.parseInt)
  ST.IfLengthPairTestTok ->
    AST.IfConditionHead <$> (AST.IfLengthPairTest <$> Par.parseLength <*> parseRelationExpanding <*> Par.parseLength)
  ST.IfIntOddTok ->
    AST.IfConditionHead <$> (AST.IfIntOdd <$> Par.parseInt)
  ST.IfInModeTok a ->
    pure $ AST.IfConditionHead $ AST.IfInMode a
  ST.IfTokenAttributesEqualTok attr ->
    AST.IfConditionHead <$> (AST.IfTokenAttributesEqual attr <$> anyLexExpanding <*> anyLexExpanding)
  ST.IfTokensEqualTok ->
    AST.IfConditionHead <$> (AST.IfTokensEqual <$> anyLexInhibited <*> anyLexInhibited)
  ST.IfBoxRegisterIsTok attr ->
    AST.IfConditionHead <$> (AST.IfBoxRegisterIs attr <$> Par.parseInt)
  ST.IfInputEndedTok ->
    AST.IfConditionHead <$> (AST.IfInputEnded <$> Par.parseInt)
  ST.IfConstTok b ->
    pure $ (AST.IfConditionHead . AST.IfConst) b
  ST.CaseTok ->
    AST.CaseConditionHead <$> Par.parseInt
