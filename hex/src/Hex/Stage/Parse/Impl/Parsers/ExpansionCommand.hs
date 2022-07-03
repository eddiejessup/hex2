module Hex.Stage.Parse.Impl.Parsers.ExpansionCommand where

import Control.Monad.Combinators qualified as PC
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.ExpandableToken qualified as ST
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Stream qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Impl.Parsers.ExpansionCommand.Condition qualified as Par
import Hex.Stage.Parse.Impl.Parsers.ExpansionCommand.MacroCall qualified as Par
import Hex.Stage.Parse.Interface.AST.ExpansionCommand qualified as AST
import Hexlude

headToParseExpansionCommand :: MonadPrimTokenParse m => ST.ExpansionCommandHeadToken -> m AST.ExpansionCommand
headToParseExpansionCommand = \case
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
    AST.ExpandAfter <$> getUnexpandedToken <*> getUnexpandedToken
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

parseCSNameBody :: MonadPrimTokenParse m => m Lex.ControlSequence
parseCSNameBody = do
  controlSequenceCodes <- PC.manyTill getCharCode (Par.satisfyEquals PT.EndCSNameTok)
  pure $ Lex.mkControlSequence controlSequenceCodes
  where
    getCharCode =
      Par.getExpandedLexToken >>= \case
        lt@(Lex.ControlSequenceLexToken _) -> parseError $ Par.SawUnexpectedLexToken $ Par.UnexpectedLexToken {saw = lt, expected = "Character token"}
        Lex.CharCatLexToken lexCharCat -> pure $ lexCharCat.lexCCChar
