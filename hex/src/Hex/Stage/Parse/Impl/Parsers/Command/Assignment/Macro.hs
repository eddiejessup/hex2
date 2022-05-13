module Hex.Stage.Parse.Impl.Parsers.Command.Assignment.Macro where

import ASCII.Decimal qualified as ASCII
import Control.Monad.Combinators qualified as PC
import Data.Sequence qualified as Seq
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

parseMacroBody :: MonadPrimTokenParse m => PT.ExpandDefFlag -> Seq PT.AssignPrefixTok -> m AST.AssignmentBody
parseMacroBody defExpandType prefixes = do
  cs <- Par.parseCSName
  tgt <- parseMacroDefinition defExpandType prefixes
  pure $ AST.DefineControlSequence cs (AST.MacroTarget tgt)

manySatisfiedLexThen :: MonadPrimTokenParse m => (Lex.LexToken -> Maybe a) -> m [a]
manySatisfiedLexThen f = PC.many $ Par.satisfyLexThen f

-- Parse a parameter specification, and also the begin-group that surround the macro's replacement text.
parseMacroParameterSpecificationAndLeftBrace :: forall m. MonadPrimTokenParse m => m ST.MacroParameterSpecification
parseMacroParameterSpecificationAndLeftBrace = do
  -- Pre-parameter text tokens.
  preParameterText <- parseMacroParameterDelimiterText
  -- Parameters, if present.
  parameterDelimiterTexts <- Par.parseHeaded $ headToParseParameterDelimiterTextsFrom ASCII.Digit1
  pure $ ST.MacroParameterSpecification {preParameterText, parameterDelimiterTexts}
  where
    headToParseParameterDelimiterTextsFrom :: ASCII.Digit -> PT.PrimitiveToken -> m (Seq ST.ParameterText)
    headToParseParameterDelimiterTextsFrom paramNum headToken =
      headToParseEndOfParams headToken <|> headToParseParamsFrom headToken
      where
        -- Parse the left-brace that indicates the end of parameters.
        headToParseEndOfParams t
          | Par.primTokenHasCategory Code.BeginGroup t =
              pure Seq.empty
          | otherwise =
              empty

        -- Parse a present parameter, then the remaining parameters, if present.
        -- Parse, for example, '#3'.
        headToParseParamsFrom t
          | Par.primTokenHasCategory Code.Parameter t = do
              -- First, we require that we see a parameter number that matches
              -- the current parameter number.
              Par.skipSatisfiedLex $ \case
                Lex.CharCatLexToken lexCC -> case lexCC.lexCCCat of
                  -- If we saw a 'letter' or 'other', precisely if it is a digit
                  -- corresponding to the next expected parameter number.
                  Code.Letter ->
                    charCodeEqDigit lexCC.lexCCChar paramNum
                  Code.Other ->
                    charCodeEqDigit lexCC.lexCCChar paramNum
                  -- A char-cat of any other category is invalid.
                  _ -> False
                -- A control sequence is invalid.
                _ -> False
              -- Parse delimiter tokens after the parameter number, if present.
              currentParameterDelimiters <- parseMacroParameterDelimiterText
              -- Return this parameter, plus any remaining parameters.
              laterResult <- case paramNum of
                -- If we are parsing parameter nine, there can't be any more, so we
                -- only expect to end the parameters.
                ASCII.Digit9 -> Par.parseHeaded headToParseEndOfParams
                -- Otherwise, we can either end the parameters, or have some more,
                -- starting from the successor of this paramNumit.
                _ -> Par.parseHeaded $ headToParseParameterDelimiterTextsFrom (succ paramNum)
              pure $ currentParameterDelimiters <| laterResult
          | otherwise =
              empty

        charCodeEqDigit :: Code.CharCode -> ASCII.Digit -> Bool
        charCodeEqDigit charCode expectedDigit = case charCodeToDigit charCode of
          Nothing -> False
          Just seenDigit -> seenDigit == expectedDigit

charCodeToDigit :: Code.CharCode -> Maybe ASCII.Digit
charCodeToDigit charCode = ASCII.toDigitMaybe (Code.codeAsAsciiChar charCode)

parseMacroParameterDelimiterText :: MonadPrimTokenParse m => m ST.ParameterText
parseMacroParameterDelimiterText = do
  delimiterTokens <- PC.many $ Par.satisfyLexIf isDelimiterToken
  pure $ ST.ParameterText $ Seq.fromList delimiterTokens
  where
    isDelimiterToken = \case
      Lex.CharCatLexToken lexCharCat -> case lexCharCat.lexCCCat of
        Code.Parameter -> False
        Code.BeginGroup -> False
        Code.EndGroup -> False
        _ -> True
      _ ->
        True

parseMacroDefinition :: MonadPrimTokenParse m => PT.ExpandDefFlag -> Seq PT.AssignPrefixTok -> m ST.MacroDefinition
parseMacroDefinition defExpandType prefixes = do
  parameterSpecification <- parseMacroParameterSpecificationAndLeftBrace
  replacementText <- parseMacroReplacementText defExpandType
  pure
    ST.MacroDefinition
      { ST.parameterSpecification,
        ST.replacementText,
        ST.long = PT.LongTok `elem` prefixes,
        ST.outer = PT.OuterTok `elem` prefixes
      }

-- Extract a balanced text but parsing parameter references at definition-time.
-- Assumes we just parsed the '{' that starts the macro text.
parseMacroReplacementText :: MonadPrimTokenParse m => PT.ExpandDefFlag -> m ST.MacroReplacementText
parseMacroReplacementText = \case
  PT.ExpandDef ->
    notImplemented "parseMacroReplacementText: ExpandDef"
  PT.InhibitDef -> do
    ST.InhibitedMacroReplacementText <$> parseInhibitedMacroReplacementText

parseInhibitedMacroReplacementText :: forall m. MonadPrimTokenParse m => m ST.InhibitedReplacementText
parseInhibitedMacroReplacementText = ST.InhibitedReplacementText . fst <$> Par.parseNestedExpr parseNext
  where
    parseNext :: Int -> m (ST.MacroTextToken, Ordering)
    parseNext _depth =
      getUnexpandedToken >>= \case
        -- If we see a '#', parse the parameter number and return a token
        -- representing the call.
        Lex.CharCatLexToken Lex.LexCharCat {lexCCCat = Code.Parameter} -> do
          textToken <- Par.satisfyLexThen paramNumOrHash
          pure (textToken, EQ)
        -- Otherwise, just return the ordinary lex token.
        lexToken ->
          pure (ST.MacroTextLexToken lexToken, Par.lexTokenToGroupDepthChange lexToken)

    -- We are happy iff the '#' is followed by either a decimal digit, or another '#'.
    paramNumOrHash :: Lex.LexToken -> Maybe ST.MacroTextToken
    paramNumOrHash = \case
      Lex.CharCatLexToken charCat -> case charCat.lexCCCat of
        -- If we see a char-cat with category 'letter' or 'other', we expect it
        -- to represent a digit.
        Code.Letter ->
          paramNumFromCharCode charCat.lexCCChar
        Code.Other -> do
          paramNumFromCharCode charCat.lexCCChar
        -- If the char-cat has category 'parameter', we have seen a double-hash.
        -- In this case we emit a lex-token (not a parameter-token), with
        -- category 'other', and the char-code of the second '#'.
        Code.Parameter ->
          Just $ ST.MacroTextLexToken $ Lex.CharCatLexToken (Lex.LexCharCat charCat.lexCCChar Code.Other)
        _ ->
          Nothing
      _ ->
        Nothing

    paramNumFromCharCode :: Code.CharCode -> Maybe ST.MacroTextToken
    paramNumFromCharCode charCode = case charCodeToDigit charCode of
      Nothing ->
        Nothing
      Just paramDigit ->
        pure $ ST.MacroTextParamToken $ ST.ParameterNumber paramDigit
