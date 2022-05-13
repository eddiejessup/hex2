module Hex.Stage.Parse.Impl.Parsers.SyntaxCommand where

import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Stream qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Impl.Parsers.SyntaxCommand.Condition qualified as Par
import Hex.Stage.Parse.Impl.Parsers.SyntaxCommand.MacroCall qualified as Par
import Hex.Stage.Parse.Interface.AST.SyntaxCommand qualified as AST
import Hexlude
import qualified Hex.Stage.Parse.Impl.Parsers.Command as Par

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
  ST.NumberTok -> do
    AST.RenderNumber <$> Par.parseInt
  ST.RomanNumeralTok -> do
    AST.RenderRomanNumeral <$> Par.parseInt
  ST.StringTok -> do
    AST.RenderTokenAsTokens <$> getUnexpandedToken
  ST.JobNameTok -> do
    pure $ AST.RenderJobName
  ST.FontNameTok -> do
    AST.RenderFontName <$> (Par.parseHeaded Par.headToParseFontRef)
  ST.MeaningTok -> do
    AST.RenderTokenMeaning <$> getUnexpandedToken
  ST.CSNameTok -> do
    byteString <- parseCSNameBody
    pure $ AST.ParseControlSequence byteString
  ST.ExpandAfterTok -> do
    AST.ExpandAfter <$> getUnexpandedToken
  ST.NoExpandTok -> do
    AST.NoExpand <$> getUnexpandedToken
  ST.MarkRegisterTok markRegister -> do
    pure $ AST.GetMarkRegister markRegister
  ST.InputTok -> do
    AST.OpenInputFile <$> Par.parseFileName
  ST.EndInputTok -> do
    pure $ AST.EndInputFile
  ST.TheTok -> do
    AST.RenderInternalQuantity <$> Par.parseHeaded Par.headToParseInternalQuantity
  ST.ChangeCaseTok vDirection -> do
    pure $ AST.ChangeCase vDirection

parseCSNameBody :: m ByteString
parseCSNameBody = notImplemented "parseCSNameBody"
