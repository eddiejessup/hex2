module Hex.Stage.Parse.Impl.Parsers.Command.Assignment.Macro where

import ASCII.Decimal qualified as ASCII
import Control.Monad.Combinators qualified as PC
import Data.Sequence qualified as Seq
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.Parse.Interface (PrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude
import qualified Hex.Capability.Log.Interface as Log

parseMacroBody :: [PrimTokenParse, EAlternative, Log.HexLog] :>> es => PT.ExpandDefFlag -> Seq PT.AssignPrefixTok -> Eff es AST.AssignmentBody
parseMacroBody defExpandType prefixes = do
  cs <- Par.parseControlSymbol
  tgt <- parseMacroDefinition defExpandType prefixes
  pure $ AST.DefineControlSequence cs (AST.MacroTarget tgt)

-- Parse a parameter specification, and also the begin-group that surround the macro's replacement text.
parseMacroParameterSpecificationAndLeftBrace :: forall es. [PrimTokenParse, EAlternative, Log.HexLog] :>> es => Eff es ST.MacroParameterSpecification
parseMacroParameterSpecificationAndLeftBrace = do
  -- Pre-parameter text tokens.
  preParameterText <- parseMacroParameterDelimiterText
  -- Parameters, if present.
  parameterDelimiterTexts <- Par.anyLexInhibited >>= headToParseParameterDelimiterTextsFrom ASCII.Digit1
  pure $ ST.MacroParameterSpecification {preParameterText, parameterDelimiterTexts}
  where
    headToParseParameterDelimiterTextsFrom :: ASCII.Digit -> LT.LexToken -> Eff es (Seq ST.ParameterText)
    headToParseParameterDelimiterTextsFrom paramNum headToken
      -- Parse the left-brace that indicates the end of parameters.
      | lexTokenEndsParameters headToken =
          pure Seq.empty
      -- Parse a present parameter, then the remaining parameters, if present.
      -- Parse, for example, '#3'.
      | Par.lexTokenHasCategory Code.Parameter headToken = do
          -- First, we require that we see a parameter number that matches
          -- the current parameter number.
          Par.skipSatisfied Par.satisfyThenInhibited $ \case
            LT.CharCatLexToken lexCC -> case lexCC.lexCCCat of
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
            ASCII.Digit9 -> do
              Par.skipSatisfied Par.satisfyThenInhibited lexTokenEndsParameters
              pure Seq.empty
            -- Otherwise, we can either end the parameters, or have some more,
            -- starting from the successor of this paramNumit.
            _ -> Par.anyLexInhibited >>= headToParseParameterDelimiterTextsFrom (succ paramNum)
          pure $ currentParameterDelimiters <| laterResult
      | otherwise =
          Par.parseFail $ "headToParseParameterDelimiterTextsFrom " <> F.sformat LT.fmtLexToken headToken

    lexTokenEndsParameters :: LT.LexToken -> Bool
    lexTokenEndsParameters = Par.lexTokenHasCategory Code.BeginGroup

    charCodeEqDigit :: Code.CharCode -> ASCII.Digit -> Bool
    charCodeEqDigit charCode expectedDigit = case charCodeToDigit charCode of
      Nothing -> False
      Just seenDigit -> seenDigit == expectedDigit

charCodeToDigit :: Code.CharCode -> Maybe ASCII.Digit
charCodeToDigit charCode = ASCII.toDigitMaybe (Code.codeAsAsciiChar charCode)

parseMacroParameterDelimiterText :: [PrimTokenParse, EAlternative, Log.HexLog] :>> es => Eff es ST.ParameterText
parseMacroParameterDelimiterText = do
  delimiterTokens <- PC.many $ Par.satisfyIf Par.satisfyThenInhibited isDelimiterToken
  pure $ ST.ParameterText $ Seq.fromList delimiterTokens
  where
    isDelimiterToken = \case
      LT.CharCatLexToken lexCharCat -> case lexCharCat.lexCCCat of
        Code.Parameter -> False
        Code.BeginGroup -> False
        Code.EndGroup -> False
        _ -> True
      _ ->
        True

parseMacroDefinition :: [PrimTokenParse, EAlternative, Log.HexLog] :>> es => PT.ExpandDefFlag -> Seq PT.AssignPrefixTok -> Eff es ST.MacroDefinition
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
parseMacroReplacementText :: forall es. [PrimTokenParse, EAlternative, Log.HexLog] :>> es => PT.ExpandDefFlag -> Eff es ST.MacroReplacementText
parseMacroReplacementText = \case
  PT.ExpandDef ->
    notImplemented "parseMacroReplacementText: ExpandDef"
  PT.InhibitDef -> do
    ST.InhibitedMacroReplacementText <$> parseInhibitedMacroReplacementText

parseInhibitedMacroReplacementText :: forall es. [PrimTokenParse, EAlternative, Log.HexLog] :>> es => Eff es ST.InhibitedReplacementText
parseInhibitedMacroReplacementText = ST.InhibitedReplacementText . fst <$> Par.parseNestedExpr parseNext
  where
    parseNext :: Int -> Eff es (ST.MacroTextToken, Ordering)
    parseNext _depth =
      Par.anyLexInhibited >>= \case
        -- If we see a '#', parse the parameter number and return a token
        -- representing the call.
        LT.CharCatLexToken LT.LexCharCat {lexCCCat = Code.Parameter} -> do
          textToken <- Par.satisfyThenInhibited paramNumOrHash
          pure (textToken, EQ)
        -- Otherwise, just return the ordinary lex token.
        lexToken ->
          pure (ST.MacroTextLexToken lexToken, Par.lexTokenToGroupDepthChange lexToken)

    -- We are happy iff the '#' is followed by either a decimal digit, or another '#'.
    paramNumOrHash :: LT.LexToken -> Maybe ST.MacroTextToken
    paramNumOrHash = \case
      LT.CharCatLexToken charCat -> case charCat.lexCCCat of
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
          Just $ ST.MacroTextLexToken $ LT.CharCatLexToken (LT.LexCharCat charCat.lexCCChar Code.Other)
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
