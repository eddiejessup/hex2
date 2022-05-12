module Hex.Stage.Parse.Impl.Parsers.SyntaxCommand.Condition where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.SyntaxCommand qualified as AST
import Hexlude

parseRelation :: MonadPrimTokenParse m => m Ordering
parseRelation = satisfyThen $ \t ->
  if
      | Par.isOnly (Par.primTokCatChar Code.Other) (Code.Chr_ '<') t -> Just LT
      | Par.isOnly (Par.primTokCatChar Code.Other) (Code.Chr_ '>') t -> Just GT
      | Par.isOnly (Par.primTokCatChar Code.Other) (Code.Chr_ '=') t -> Just EQ
      | otherwise -> Nothing

parseConditionHead :: MonadPrimTokenParse m => ST.ConditionHeadTok -> m AST.ConditionHead
parseConditionHead = \case
  ST.IfIntPairTestTok ->
    AST.IfConditionHead <$> (AST.IfIntPairTest <$> Par.parseInt <*> parseRelation <*> Par.parseInt)
  ST.IfLengthPairTestTok ->
    AST.IfConditionHead <$> (AST.IfLengthPairTest <$> Par.parseLength <*> parseRelation <*> Par.parseLength)
  ST.IfIntOddTok ->
    AST.IfConditionHead <$> (AST.IfIntOdd <$> Par.parseInt)
  ST.IfInModeTok a ->
    pure $ AST.IfConditionHead $ AST.IfInMode a
  ST.IfTokenAttributesEqualTok attr ->
    AST.IfConditionHead <$> (AST.IfTokenAttributesEqual attr <$> getAnyPrimitiveToken <*> getAnyPrimitiveToken)
  ST.IfTokensEqualTok ->
    AST.IfConditionHead <$> (AST.IfTokensEqual <$> getAnyLexToken <*> getAnyLexToken)
  ST.IfBoxRegisterIsTok attr ->
    AST.IfConditionHead <$> (AST.IfBoxRegisterIs attr <$> Par.parseInt)
  ST.IfInputEndedTok ->
    AST.IfConditionHead <$> (AST.IfInputEnded <$> Par.parseInt)
  ST.IfConstTok b ->
    pure $ (AST.IfConditionHead . AST.IfConst) b
  ST.CaseTok ->
    AST.CaseConditionHead <$> Par.parseInt
