module Hex.Stage.Parse.Impl.Parsers.ExpansionCommand where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Stream qualified as Par
import Hex.Stage.Parse.Impl.Parsers.ExpansionCommand.Condition qualified as Par
import Hex.Stage.Parse.Impl.Parsers.ExpansionCommand.MacroCall qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
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
    AST.RenderTokenAsTokens <$> anyLexInhibited
  ST.JobNameTok ->
    pure $ AST.RenderJobName
  ST.FontNameTok ->
    AST.RenderFontName <$> (anyPrim >>= Par.headToParseFontRef)
  ST.MeaningTok ->
    AST.RenderTokenMeaning <$> anyLexInhibited
  ST.CSNameTok ->
    AST.ParseControlSequence <$> parseControlSymbolBody
  ST.ExpandAfterTok ->
    AST.ExpandAfter <$> anyLexInhibited <*> anyLexInhibited
  ST.NoExpandTok ->
    AST.NoExpand <$> anyLexInhibited
  ST.MarkRegisterTok markRegister ->
    pure $ AST.GetMarkRegister markRegister
  ST.InputTok ->
    AST.OpenInputFile <$> Par.parseFileName
  ST.EndInputTok ->
    pure $ AST.EndInputFile
  ST.TheTok ->
    AST.RenderInternalQuantity <$> (anyPrim >>= Par.headToParseInternalQuantity)
  ST.ChangeCaseTok vDirection ->
    AST.ChangeCase vDirection <$> (Par.parseInhibitedGeneralText Par.ExpectingBeginGroup)

parseControlSymbolBody :: MonadPrimTokenParse m => m LT.ControlSequence
parseControlSymbolBody = do
  controlSequenceCodes <- PC.manyTill getCharCode (satisfyEquals PT.EndCSNameTok)
  pure $ LT.mkControlSequence controlSequenceCodes
  where
    getCharCode =
      satisfyLexThenExpanding $ \case
        LT.ControlSequenceLexToken _ -> Nothing
        LT.CharCatLexToken lexCharCat -> Just lexCharCat.lexCCChar
