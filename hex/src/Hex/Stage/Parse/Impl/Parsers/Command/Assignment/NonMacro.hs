module Hex.Stage.Parse.Impl.Parsers.Command.Assignment.NonMacro where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Codes qualified as Code
-- import Hex.Stage.Parse.Impl.Parsers.Quantity.MathGlue qualified as Par
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..), parseFailure)
import Hex.Common.Parse.Interface qualified as Par
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
  MonadPrimTokenParse m =>
  T.PrimitiveToken ->
  m AST.AssignmentBody
headToParseNonMacroAssignmentBody = \case
  T.HyphenationTok ->
    AST.SetHyphenation <$> Par.parseInhibitedGeneralText Par.ExpectingBeginGroup
  T.HyphenationPatternsTok ->
    AST.SetHyphenationPatterns <$> Par.parseInhibitedGeneralText Par.ExpectingBeginGroup
  T.InteractionModeTok mode ->
    pure $ AST.SetInteractionMode mode
  T.LetTok -> do
    (cs, tgt) <- parseXEqualsY NotExpanding parseCSName (skipOneOptionalSpace NotExpanding >> getUnexpandedToken)
    pure $ AST.DefineControlSequence cs $ AST.LetTarget tgt
  T.FutureLetTok -> do
    cs <- parseCSName
    lt1 <- getUnexpandedToken
    lt2 <- getUnexpandedToken
    pure $
      AST.DefineControlSequence cs $
        AST.FutureLetTarget $
          AST.FutureLetTargetDefinition
            { tokenToExpand = lt1,
              letTargetToken = lt2
            }
  T.ShortDefHeadTok quant -> do
    (cs, nr) <- parseXEqualsY Expanding parseCSName Par.parseInt
    pure $ AST.DefineControlSequence cs (AST.ShortDefineTarget quant nr)
  T.ParagraphShapeTok -> do
    skipOptionalEquals Expanding
    -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
    -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
    -- consecutive occurrences of ⟨dimen⟩
    nrPairs <- Par.parseInt
    shapeLengths <- PC.many Par.parseLength
    pure $ AST.SetParShape nrPairs shapeLengths
  T.ReadTok -> do
    nr <- Par.parseInt
    skipKeyword Expanding [Code.Chr_ 't', Code.Chr_ 'o']
    skipOptionalSpaces Expanding
    cs <- parseCSName
    pure $ AST.DefineControlSequence cs (AST.ReadTarget nr)
  T.SetBoxRegisterTok -> do
    (var, box) <- parseXEqualsY Expanding Par.parseInt (skipFillerExpanding >> (Par.getExpandedPrimitiveToken >>= Par.headToParseBox))
    pure $ AST.SetBoxRegister var box
  -- \font <control-sequence> <equals> <file-name> <at-clause>
  T.FontTok -> do
    (cs, tgt) <- parseXEqualsY Expanding parseCSName parseFontTarget
    pure $ AST.DefineControlSequence cs (AST.FontTarget tgt)
  t ->
    choiceFlap
      [ headToParseCodeAssignment,
        fmap AST.ModifyVariable <$> headToParseModifyVariable,
        fmap AST.SetVariable <$> headToParseVariableAssignment,
        fmap AST.SelectFont <$> Par.headToParseFontRefToken,
        headToParseSetFamilyMember,
        headToParseSetFontDimension,
        headToParseSetFontChar,
        headToParseSetBoxDimension
      ]
      t

parseFontTarget :: MonadPrimTokenParse m => m AST.FontFileSpec
parseFontTarget = do
  fname <- Par.parseFileName
  fontSpec <-
    PC.choice
      [ do
          skipKeyword Expanding [Code.Chr_ 'a', Code.Chr_ 't']
          AST.FontAt <$> Par.parseLength,
        do
          skipKeyword Expanding [Code.Chr_ 's', Code.Chr_ 'c', Code.Chr_ 'a', Code.Chr_ 'l', Code.Chr_ 'e', Code.Chr_ 'd']
          AST.FontScaled <$> Par.parseInt,
        do
          skipOptionalSpaces Expanding
          pure AST.NaturalFont
      ]
  pure $ AST.FontFileSpec fontSpec fname

headToParseCodeAssignment :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.AssignmentBody
headToParseCodeAssignment t = do
  (ref, tgt) <- parseXEqualsY Expanding (Par.headToParseCodeTableRef t) Par.parseInt
  pure $ AST.AssignCode $ AST.CodeAssignment ref tgt

headToParseModifyVariable :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.VariableModification
headToParseModifyVariable = \case
  T.AdvanceVarTok ->
    PC.choice
      [ do
          var <- Par.getExpandedPrimitiveToken >>= Par.headToParseIntVariable
          skipOptionalBy
          AST.AdvanceIntVariable var <$> Par.parseInt,
        do
          var <- Par.getExpandedPrimitiveToken >>= Par.headToParseLengthVariable
          skipOptionalBy
          AST.AdvanceLengthVariable var <$> Par.parseLength,
        do
          var <- Par.getExpandedPrimitiveToken >>= Par.headToParseGlueVariable
          skipOptionalBy
          AST.AdvanceGlueVariable var <$> Par.parseGlue,
        do
          var <- Par.getExpandedPrimitiveToken >>= Par.headToParseMathGlueVariable
          skipOptionalBy
          AST.AdvanceMathGlueVariable var <$> Par.parseMathGlue
      ]
  T.ScaleVarTok d -> do
    var <- parseNumericVariable
    skipOptionalBy
    AST.ScaleVariable d var <$> Par.parseInt
  _ ->
    parseFailure "headToParseModifyVariable"

parseNumericVariable :: MonadPrimTokenParse m => m AST.NumericVariable
parseNumericVariable =
  PC.choice
    [ AST.IntNumericVariable <$> (Par.getExpandedPrimitiveToken >>= Par.headToParseIntVariable),
      AST.LengthNumericVariable <$> (Par.getExpandedPrimitiveToken >>= Par.headToParseLengthVariable),
      AST.GlueNumericVariable <$> (Par.getExpandedPrimitiveToken >>= Par.headToParseGlueVariable),
      AST.MathGlueNumericVariable <$> (Par.getExpandedPrimitiveToken >>= Par.headToParseMathGlueVariable)
    ]

skipOptionalBy :: MonadPrimTokenParse m => m ()
skipOptionalBy =
  PC.choice
    [ void $ parseOptionalKeyword Expanding [Code.Chr_ 'b', Code.Chr_ 'y'],
      skipOptionalSpaces Expanding
    ]

headToParseVariableAssignment :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.VariableAssignment
headToParseVariableAssignment t =
  PC.choice
    [ do
        (var, tgt) <- parseXEqualsY Expanding (Par.headToParseIntVariable t) Par.parseInt
        pure $ AST.IntVariableAssignment $ AST.QuantVariableAssignment var tgt,
      do
        (var, tgt) <- parseXEqualsY Expanding (Par.headToParseLengthVariable t) Par.parseLength
        pure $ AST.LengthVariableAssignment $ AST.QuantVariableAssignment var tgt,
      do
        (var, tgt) <- parseXEqualsY Expanding (Par.headToParseGlueVariable t) Par.parseGlue
        pure $ AST.GlueVariableAssignment $ AST.QuantVariableAssignment var tgt,
      do
        (var, tgt) <- parseXEqualsY Expanding (Par.headToParseMathGlueVariable t) Par.parseMathGlue
        pure $ AST.MathGlueVariableAssignment $ AST.QuantVariableAssignment var tgt,
      do
        (var, tgt) <- parseXEqualsY Expanding (Par.headToParseTokenListVariable t) parseTokenListTarget
        pure $ AST.TokenListVariableAssignment $ AST.QuantVariableAssignment var tgt,
      -- Unofficial variable assignments, separated because of being
      -- global in the TeXbook.
      do
        (var, tgt) <- parseXEqualsY Expanding (Par.headToParseSpecialInt t) Par.parseInt
        pure $ AST.SpecialIntParameterVariableAssignment var tgt,
      do
        (var, tgt) <- parseXEqualsY Expanding (Par.headToParseSpecialLength t) Par.parseLength
        pure $ AST.SpecialLengthParameterVariableAssignment var tgt
    ]

parseTokenListTarget :: MonadPrimTokenParse m => m AST.TokenListAssignmentTarget
parseTokenListTarget =
  PC.choice
    [ AST.TokenListAssignmentText <$> Par.parseInhibitedGeneralText Par.ExpectingBeginGroup,
      do
        skipFillerExpanding
        AST.TokenListAssignmentVar <$> (Par.getExpandedPrimitiveToken >>= Par.headToParseTokenListVariable)
    ]

headToParseSetFamilyMember :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.AssignmentBody
headToParseSetFamilyMember t = do
  (var, val) <- parseXEqualsY Expanding (Par.headToParseFamilyMember t) (Par.getExpandedPrimitiveToken >>= Par.headToParseFontRef)
  pure $ AST.SetFamilyMember var val

headToParseSetFontDimension :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.AssignmentBody
headToParseSetFontDimension t = do
  (var, val) <- parseXEqualsY Expanding (Par.headToParseFontDimensionRef t) Par.parseLength
  pure $ AST.SetFontDimension var val

headToParseSetFontChar :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.AssignmentBody
headToParseSetFontChar t = do
  (var, val) <- parseXEqualsY Expanding (Par.headToParseFontCharRef t) Par.parseInt
  pure $ AST.SetFontChar var val

headToParseSetBoxDimension :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.AssignmentBody
headToParseSetBoxDimension t = do
  (var, val) <- parseXEqualsY Expanding (Par.headToParseBoxDimensionRef t) Par.parseLength
  pure $ AST.SetBoxDimension var val
