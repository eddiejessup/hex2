module Hex.Stage.Parse.Impl.Parsers.Command.Assignment.NonMacro where

import Control.Monad.Combinators qualified as PC
-- import Hex.Stage.Parse.Impl.Parsers.Quantity.MathGlue qualified as Par

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Hyphen qualified as HSt.Hyph
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Common.Token.Resolved.Primitive qualified as T
import Hex.Stage.Expand.Interface (PrimTokenSource (..), parseFail)
import Hex.Stage.Expand.Interface qualified as Par
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Command.Box qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Stream qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Glue qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.MathGlue qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

headToParseNonMacroAssignmentBody ::
  [PrimTokenSource, EAlternative, Log.HexLog] :>> es =>
  T.PrimitiveToken ->
  Eff es AST.AssignmentBody
headToParseNonMacroAssignmentBody = \case
  T.HyphenationTok ->
    AST.SetHyphenation <$> parseHyphenationExceptions
  T.HyphenationPatternsTok ->
    AST.SetHyphenationPatterns <$> parseHyphenationPatterns
  T.InteractionModeTok mode ->
    pure $ AST.SetInteractionMode mode
  T.LetTok -> do
    (cs, tgt) <- parseXEqualsY PT.Inhibited parseControlSymbol (skipOneOptionalSpace PT.Inhibited >> anyLexInhibited)
    pure $ AST.DefineControlSequence cs $ AST.LetTarget tgt
  T.FutureLetTok -> do
    cs <- parseControlSymbol
    lt1 <- anyLexInhibited
    lt2 <- anyLexInhibited
    pure $
      AST.DefineControlSequence cs $
        AST.FutureLetTarget $
          AST.FutureLetTargetDefinition
            { tokenToExpand = lt1,
              letTargetToken = lt2
            }
  T.ShortDefHeadTok quant -> do
    (cs, nr) <- parseXEqualsY PT.Expanding parseControlSymbol Par.parseInt
    pure $ AST.DefineControlSequence cs (AST.ShortDefineTarget (AST.ShortDefTargetValue quant nr))
  T.ParagraphShapeTok -> do
    skipOptionalEquals PT.Expanding
    -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
    -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
    -- consecutive occurrences of ⟨dimen⟩
    nrPairs <- Par.parseInt
    shapeLengths <- PC.many Par.parseLength
    pure $ AST.SetParShape nrPairs shapeLengths
  T.ReadTok -> do
    nr <- Par.parseInt
    skipKeyword PT.Expanding [Code.Chr_ 't', Code.Chr_ 'o']
    skipOptionalSpaces PT.Expanding
    cs <- parseControlSymbol
    pure $ AST.DefineControlSequence cs (AST.ReadTarget nr)
  T.SetBoxRegisterTok -> do
    (var, box) <- parseXEqualsY PT.Expanding Par.parseExplicitRegisterLocation (skipFillerExpanding >> (anyPrim >>= Par.headToParseBox))
    pure $ AST.SetBoxRegister var box
  -- \font <control-sequence> <equals> <file-name> <at-clause>
  T.FontTok -> do
    (cs, tgt) <- parseXEqualsY PT.Expanding parseControlSymbol parseFontTarget
    pure $ AST.DefineControlSequence cs (AST.FontTarget tgt)
  t ->
    choiceFlap
      [ headToParseCodeAssignment,
        fmap AST.ModifyVariable <$> headToParseModifyVariable,
        fmap AST.SetVariable <$> headToParseVariableAssignment,
        fmap AST.SelectFont <$> Par.headToParseFontRefToken,
        headToParseSetFamilyMember,
        headToParseSetFontDimension,
        headToParseSetFontSpecialChar,
        headToParseSetBoxDimension
      ]
      t

parseFontTarget :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.FontFileSpec
parseFontTarget = do
  fname <- Par.parseFileName
  fontSpec <-
    PC.choice
      [ do
          skipKeyword PT.Expanding [Code.Chr_ 'a', Code.Chr_ 't']
          AST.FontAt <$> Par.parseLength,
        do
          skipKeyword PT.Expanding [Code.Chr_ 's', Code.Chr_ 'c', Code.Chr_ 'a', Code.Chr_ 'l', Code.Chr_ 'e', Code.Chr_ 'd']
          AST.FontScaled <$> Par.parseInt,
        do
          skipOptionalSpaces PT.Expanding
          pure AST.NaturalFont
      ]
  pure $ AST.FontFileSpec fontSpec fname

headToParseCodeAssignment :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => T.PrimitiveToken -> Eff es AST.AssignmentBody
headToParseCodeAssignment t = do
  Log.debugLog $ "Trying to parse code assignment " <> F.sformat PT.fmtPrimitiveToken t
  (ref, tgt) <- parseXEqualsY PT.Expanding (Par.headToParseCodeTableRef t) Par.parseInt
  pure $ AST.AssignCode $ AST.CodeAssignment ref tgt

headToParseModifyVariable :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => T.PrimitiveToken -> Eff es AST.VariableModification
headToParseModifyVariable = \case
  T.ModifyVariableTok modTok -> do
    argVariable <- parseNumericVariable
    skipOptionalBy
    case modTok of
      T.AdvanceVarTok ->
        case argVariable of
          AST.IntNumericVariable var ->
            AST.AdvanceIntVariable var <$> Par.parseInt
          AST.LengthNumericVariable var ->
            AST.AdvanceLengthVariable var <$> Par.parseLength
          AST.GlueNumericVariable var ->
            AST.AdvanceGlueVariable var <$> Par.parseGlue
          AST.MathGlueNumericVariable var ->
            AST.AdvanceMathGlueVariable var <$> Par.parseMathGlue
      T.ScaleVarTok d -> do
        AST.ScaleVariable d argVariable <$> Par.parseInt
  t ->
    parseFail $ "headToParseModifyVariable " <> F.sformat PT.fmtPrimitiveToken t

parseNumericVariable :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.NumericVariable
parseNumericVariable = do
  varHead <- anyPrim
  PC.choice
    [ AST.IntNumericVariable <$> Par.headToParseIntVariable varHead,
      AST.LengthNumericVariable <$> Par.headToParseLengthVariable varHead,
      AST.GlueNumericVariable <$> Par.headToParseGlueVariable varHead,
      AST.MathGlueNumericVariable <$> Par.headToParseMathGlueVariable varHead
    ]

skipOptionalBy :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es ()
skipOptionalBy =
  PC.choice
    [ void $ parseOptionalKeyword PT.Expanding [Code.Chr_ 'b', Code.Chr_ 'y'],
      skipOptionalSpaces PT.Expanding
    ]

headToParseVariableAssignment :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => T.PrimitiveToken -> Eff es AST.VariableAssignment
headToParseVariableAssignment t = do
  Log.debugLog $ "Parsing headToParseVariableAssignment with head: " <> F.sformat PT.fmtPrimitiveToken t
  PC.choice
    [ do
        Log.debugLog "Parsing 'IntVariable' assignment"
        (var, tgt) <- parseXEqualsY PT.Expanding (Par.headToParseIntVariable t) Par.parseInt
        pure $ AST.IntVariableAssignment $ AST.QuantVariableAssignment var tgt,
      do
        Log.debugLog "Parsing 'LengthVariable' assignment"
        (var, tgt) <- parseXEqualsY PT.Expanding (Par.headToParseLengthVariable t) Par.parseLength
        pure $ AST.LengthVariableAssignment $ AST.QuantVariableAssignment var tgt,
      do
        Log.debugLog "Parsing 'GlueVariable' assignment"
        (var, tgt) <- parseXEqualsY PT.Expanding (Par.headToParseGlueVariable t) Par.parseGlue
        pure $ AST.GlueVariableAssignment $ AST.QuantVariableAssignment var tgt,
      do
        Log.debugLog "Parsing 'MathGlueVariable' assignment"
        (var, tgt) <- parseXEqualsY PT.Expanding (Par.headToParseMathGlueVariable t) Par.parseMathGlue
        pure $ AST.MathGlueVariableAssignment $ AST.QuantVariableAssignment var tgt,
      do
        Log.debugLog "Parsing 'TokenListVariable' assignment"
        (var, tgt) <- parseXEqualsY PT.Expanding (Par.headToParseTokenListVariable t) parseTokenListTarget
        pure $ AST.TokenListVariableAssignment $ AST.QuantVariableAssignment var tgt,
      -- Unofficial variable assignments, separated because of being
      -- global in the TeXbook.
      do
        Log.debugLog "Parsing 'SpecialInt' assignment"
        (var, tgt) <- parseXEqualsY PT.Expanding (Par.headToParseSpecialInt t) Par.parseInt
        pure $ AST.SpecialIntParameterVariableAssignment var tgt,
      do
        Log.debugLog "Parsing 'SpecialLength' assignment"
        (var, tgt) <- parseXEqualsY PT.Expanding (Par.headToParseSpecialLength t) Par.parseLength
        pure $ AST.SpecialLengthParameterVariableAssignment var tgt
    ]

parseTokenListTarget :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.TokenListAssignmentTarget
parseTokenListTarget =
  PC.choice
    [ AST.TokenListAssignmentText <$> Par.parseInhibitedGeneralText Par.ExpectingBeginGroup,
      do
        skipFillerExpanding
        AST.TokenListAssignmentVar <$> (anyPrim >>= Par.headToParseTokenListVariable)
    ]

headToParseSetFamilyMember :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => T.PrimitiveToken -> Eff es AST.AssignmentBody
headToParseSetFamilyMember t = do
  (var, val) <- parseXEqualsY PT.Expanding (Par.headToParseFamilyMember t) (anyPrim >>= Par.headToParseFontRef)
  pure $ AST.SetFamilyMember var val

headToParseSetFontDimension :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => T.PrimitiveToken -> Eff es AST.AssignmentBody
headToParseSetFontDimension t = do
  (var, val) <- parseXEqualsY PT.Expanding (Par.headToParseFontDimensionRef t) Par.parseLength
  pure $ AST.SetFontDimension var val

headToParseSetFontSpecialChar :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => T.PrimitiveToken -> Eff es AST.AssignmentBody
headToParseSetFontSpecialChar t = do
  (var, val) <- parseXEqualsY PT.Expanding (Par.headToParseFontSpecialCharRef t) Par.parseInt
  pure $ AST.SetFontSpecialChar var val

headToParseSetBoxDimension :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => T.PrimitiveToken -> Eff es AST.AssignmentBody
headToParseSetBoxDimension t = do
  (var, val) <- parseXEqualsY PT.Expanding (Par.headToParseBoxDimensionRef t) Par.parseLength
  pure $ AST.SetBoxDimension var val

parseHyphenationPatterns :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es [HSt.Hyph.HyphenationPattern]
parseHyphenationPatterns = do
  skipCharCatWithCategory PT.Expanding Code.BeginGroup
  skipOptionalSpaces PT.Expanding
  patterns <- PC.sepBy parseHyphenationPattern (skipSpace PT.Expanding)
  skipCharCatWithCategory PT.Expanding Code.EndGroup
  pure patterns
  where
    parseDigit = do
      PC.optional (satisfyCharCatThen PT.Expanding Par.decCharToWord) <&> \case
        Nothing -> 0
        Just v -> v

    parseHyphenationPattern = do
      digLetPairList <- Par.tryParse $
        PC.some $ do
          dig <- parseDigit
          letter <- satisfyCharCatThen PT.Expanding $ \lexCharCat ->
            -- Exception: The character ‘.’ is treated as if it were a ⟨letter⟩
            -- of code 0 when it appears in a pattern. Code 0 (which obviously
            -- cannot match a nonzero \lccode) is used by TEX to represent the
            -- left or right edge of a word when it is being hyphenated.
            if lexCharCat.lexCCChar == Code.Chr_ '.'
              then Just lexCharCat.lexCCChar
              else case lexCharCat.lexCCCat of
                Code.Letter -> Just lexCharCat.lexCCChar
                Code.Other -> Just lexCharCat.lexCCChar
                _ -> Nothing
          pure (dig, letter)
      let digLetPairListNE = case nonEmpty digLetPairList of
            Nothing -> panic "Impossible!"
            Just a -> a
      lastDigit <- parseDigit
      pure $ HSt.Hyph.HyphenationPattern digLetPairListNE lastDigit

parseHyphenationExceptions ::
  [PrimTokenSource, EAlternative, Log.HexLog] :>> es =>
  Eff es [AST.HyphenationException]
parseHyphenationExceptions = do
  skipCharCatWithCategory PT.Expanding Code.BeginGroup
  skipOptionalSpaces PT.Expanding
  hyphExceptions <- PC.sepBy parseHyphenationException (skipSpace PT.Expanding)
  skipCharCatWithCategory PT.Expanding Code.EndGroup
  pure hyphExceptions
  where
    parseHyphenationException = do
      resList <- PC.some $
        satisfyCharCatThen PT.Expanding $
          \lexCharCat ->
            if lexCharCat == LT.LexCharCat (Code.Chr_ '-') (Code.Other)
              then Just Nothing
              else case lexCharCat.lexCCCat of
                Code.Letter -> Just $ Just lexCharCat.lexCCChar
                Code.Other -> Just $ Just lexCharCat.lexCCChar
                _ -> Nothing
      let resListNE = case nonEmpty resList of
            Nothing -> panic "Impossible!"
            Just a -> a
      pure $ AST.HyphenationException resListNE
