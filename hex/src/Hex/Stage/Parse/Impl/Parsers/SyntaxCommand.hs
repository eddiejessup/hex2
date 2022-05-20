module Hex.Stage.Parse.Impl.Parsers.SyntaxCommand where

import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Stream qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Impl.Parsers.SyntaxCommand.Condition qualified as Par
import Hex.Stage.Parse.Impl.Parsers.SyntaxCommand.MacroCall qualified as Par
import Hex.Stage.Parse.Interface.AST.SyntaxCommand qualified as AST
import Hexlude

headToParseSyntaxCommand :: MonadPrimTokenParse m => ST.SyntaxCommandHeadToken -> m AST.SyntaxCommand
headToParseSyntaxCommand = \case
  ST.MacroTok macroDefinition -> do
    args <- Par.parseMacroArguments macroDefinition.parameterSpecification
    pure $ AST.CallMacro macroDefinition args
  ST.ConditionTok conditionTok -> case conditionTok of
    ST.ConditionHeadTok conditionHeadTok ->
      AST.ApplyConditionHead <$> Par.parseConditionHead conditionHeadTok
    ST.ConditionBodyTok conditionBodyTok ->
      pure $ AST.ApplyConditionBody conditionBodyTok
  ST.NumberTok ->
    AST.RenderNumber <$> Par.parseInt
  ST.RomanNumeralTok ->
    AST.RenderRomanNumeral <$> Par.parseInt
  ST.StringTok ->
    AST.RenderTokenAsTokens <$> getUnexpandedToken
  ST.JobNameTok ->
    pure $ AST.RenderJobName
  ST.FontNameTok ->
    AST.RenderFontName <$> (Par.getExpandedPrimitiveToken >>= Par.headToParseFontRef)
  ST.MeaningTok ->
    AST.RenderTokenMeaning <$> getUnexpandedToken
  ST.CSNameTok ->
    AST.ParseControlSequence <$> parseCSNameBody
  ST.ExpandAfterTok ->
    AST.ExpandAfter <$> getUnexpandedToken
  ST.NoExpandTok ->
    AST.NoExpand <$> getUnexpandedToken
  ST.MarkRegisterTok markRegister ->
    pure $ AST.GetMarkRegister markRegister
  ST.InputTok ->
    AST.OpenInputFile <$> Par.parseFileName
  ST.EndInputTok ->
    pure $ AST.EndInputFile
  ST.TheTok ->
    AST.RenderInternalQuantity <$> (Par.getExpandedPrimitiveToken >>= Par.headToParseInternalQuantity)
  ST.ChangeCaseTok vDirection ->
    AST.ChangeCase vDirection <$> (Par.parseInhibitedGeneralText Par.ExpectingBeginGroup)

parseCSNameBody :: m ByteString
parseCSNameBody = notImplemented "parseCSNameBody"
