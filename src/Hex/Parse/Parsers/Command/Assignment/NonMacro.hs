module Hex.Parse.Parsers.Command.Assignment.NonMacro where

import Control.Monad.Combinators qualified as PC
import Hex.Codes qualified as H.C
import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Syntax.Font qualified as H.Syn
import Hex.Parse.Syntax.Command qualified as H.Par.Syn
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.BalancedText qualified as Par
import Hex.Parse.Parsers.Combinators qualified as Par
import Hex.Parse.Parsers.Command.Box qualified as Par
import Hex.Parse.Parsers.Command.Stream qualified as Par
import Hex.Parse.Parsers.Quantity.Glue qualified as Par
import Hex.Parse.Parsers.Quantity.Length qualified as Par
import Hex.Parse.Parsers.Quantity.Number qualified as Par
-- import Hex.Parse.Parsers.Quantity.MathGlue qualified as Par
import Hex.Symbol.Token.Primitive qualified as T
import Hexlude

headToParseNonMacroAssignmentBody ::
  MonadPrimTokenSource m =>
  T.PrimitiveToken ->
  m (H.Syn.AssignmentBody 'H.Syn.Parsed)
headToParseNonMacroAssignmentBody = \case
  T.HyphenationTok ->
    H.Syn.SetHyphenation <$> Par.parseInhibitedGeneralText
  T.HyphenationPatternsTok ->
    H.Syn.SetHyphenationPatterns <$> Par.parseInhibitedGeneralText
  T.InteractionModeTok mode ->
    pure $ H.Syn.SetInteractionMode mode
  T.LetTok -> do
    (cs, tgt) <- Par.parseXEqualsY Par.parseCSName (Par.skipOneOptionalSpace >> Par.fetchInhibitedLexToken)
    pure $ H.Syn.DefineControlSequence cs $ H.Par.Syn.LetTarget tgt
  T.FutureLetTok -> do
    cs <- Par.parseCSName
    lt1 <- Par.fetchInhibitedLexToken
    lt2 <- Par.fetchInhibitedLexToken
    pure $ H.Syn.DefineControlSequence cs $ H.Par.Syn.FutureLetTarget lt1 lt2
  T.ShortDefHeadTok quant -> do
    (cs, nr) <- Par.parseXEqualsY Par.parseCSName Par.parseInt
    pure $ H.Syn.DefineControlSequence cs (H.Par.Syn.ShortDefineTarget quant nr)
  T.ParagraphShapeTok -> do
    Par.skipOptionalEquals
    -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
    -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
    -- consecutive occurrences of ⟨dimen⟩
    nrPairs <- Par.parseInt
    shapeLengths <- PC.many Par.parseLength
    pure $ H.Syn.SetParShape nrPairs shapeLengths
  T.ReadTok -> do
    nr <- Par.parseInt
    Par.skipKeyword [H.C.Chr_ 't', H.C.Chr_ 'o']
    Par.skipOptionalSpaces
    cs <- Par.parseCSName
    pure $ H.Syn.DefineControlSequence cs (H.Par.Syn.ReadTarget nr)
  T.SetBoxRegisterTok -> do
    (var, box) <- Par.parseXEqualsY Par.parseInt (Par.skipFiller >> Par.parseHeaded Par.headToParseBox)
    pure $ H.Syn.SetBoxRegister var box
  -- \font <control-sequence> <equals> <file-name> <at-clause>
  T.FontTok -> do
    (cs, tgt) <- Par.parseXEqualsY Par.parseCSName parseFontTarget
    pure $ H.Syn.DefineControlSequence cs (H.Par.Syn.FontTarget tgt)
  t ->
    Par.choiceFlap
      [ headToParseCodeAssignment,
        fmap H.Syn.ModifyVariable <$> headToParseModifyVariable,
        fmap H.Syn.SetVariable <$> headToParseVariableAssignment,
        fmap H.Syn.SelectFont <$> Par.headToParseFontRefToken,
        headToParseSetFamilyMember,
        headToParseSetFontDimension,
        headToParseSetFontChar,
        headToParseSetBoxDimension
      ]
      t

parseFontTarget :: MonadPrimTokenSource m => m (H.Syn.FontFileSpec 'H.Syn.Parsed)
parseFontTarget = do
  fname <- Par.parseFileName
  fontSpec <-
    PC.choice
      [ do
          Par.skipKeyword [H.C.Chr_ 'a', H.C.Chr_ 't']
          H.Syn.FontAt <$> Par.parseLength,
        do
          Par.skipKeyword [H.C.Chr_ 's', H.C.Chr_ 'c', H.C.Chr_ 'a', H.C.Chr_ 'l', H.C.Chr_ 'e', H.C.Chr_ 'd']
          H.Syn.FontScaled <$> Par.parseInt,
        do
          Par.skipOptionalSpaces
          pure H.Syn.NaturalFont
      ]
  pure $ H.Syn.FontFileSpec fontSpec fname

headToParseCodeAssignment :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.AssignmentBody 'H.Syn.Parsed)
headToParseCodeAssignment t = do
  (ref, tgt) <- Par.parseXEqualsY (Par.headToParseCodeTableRef t) Par.parseInt
  pure $ H.Syn.AssignCode $ H.Syn.CodeAssignment ref tgt

headToParseModifyVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.VariableModification 'H.Syn.Parsed)
headToParseModifyVariable = \case
  T.AdvanceVarTok ->
    PC.choice
      [ do
          var <- Par.parseHeaded Par.headToParseIntVariable
          skipOptionalBy
          H.Syn.AdvanceIntVariable var <$> Par.parseInt,
        do
          var <- Par.parseHeaded Par.headToParseLengthVariable
          skipOptionalBy
          H.Syn.AdvanceLengthVariable var <$> Par.parseLength,
        do
          var <- Par.parseHeaded Par.headToParseGlueVariable
          skipOptionalBy
          H.Syn.AdvanceGlueVariable var <$> Par.parseGlue
          -- , do
          --     var <- Par.parseHeaded Par.headToParseMathGlueVariable
          --     skipOptionalBy
          --     H.Syn.AdvanceMathGlueVariable var <$> Par.parseMathGlue
      ]
  T.ScaleVarTok d -> do
    var <- parseNumericVariable
    skipOptionalBy
    H.Syn.ScaleVariable d var <$> Par.parseInt
  _ ->
    empty

parseNumericVariable :: MonadPrimTokenSource m => m (H.Syn.NumericVariable 'H.Syn.Parsed)
parseNumericVariable =
  PC.choice
    [ H.Syn.IntNumericVariable <$> Par.parseHeaded Par.headToParseIntVariable,
      H.Syn.LengthNumericVariable <$> Par.parseHeaded Par.headToParseLengthVariable,
      H.Syn.GlueNumericVariable <$> Par.parseHeaded Par.headToParseGlueVariable,
      H.Syn.MathGlueNumericVariable <$> Par.parseHeaded Par.headToParseMathGlueVariable
    ]

skipOptionalBy :: MonadPrimTokenSource m => m ()
skipOptionalBy =
  PC.choice
    [ void $ Par.parseOptionalKeyword [H.C.Chr_ 'b', H.C.Chr_ 'y'],
      Par.skipOptionalSpaces
    ]

headToParseVariableAssignment :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.VariableAssignment 'H.Syn.Parsed)
headToParseVariableAssignment t =
  PC.choice
    [ do
        (var, tgt) <- Par.parseXEqualsY (Par.headToParseIntVariable t) Par.parseInt
        pure $ H.Syn.IntVariableAssignment $ H.Syn.QuantVariableAssignment var tgt,
      do
        (var, tgt) <- Par.parseXEqualsY (Par.headToParseLengthVariable t) Par.parseLength
        pure $ H.Syn.LengthVariableAssignment $ H.Syn.QuantVariableAssignment var tgt,
      do
        (var, tgt) <- Par.parseXEqualsY (Par.headToParseGlueVariable t) Par.parseGlue
        pure $ H.Syn.GlueVariableAssignment $ H.Syn.QuantVariableAssignment var tgt,
      -- do
      --   (var, tgt) <- Par.parseXEqualsY (Par.headToParseMathGlueVariable t) Par.parseMathGlue
      --   pure $ H.Syn.MathGlueVariableAssignment $ H.Syn.QuantVariableAssignment var tgt,
      do
        (var, tgt) <- Par.parseXEqualsY (Par.headToParseTokenListVariable t) parseTokenListTarget
        pure $ H.Syn.TokenListVariableAssignment $ H.Syn.QuantVariableAssignment var tgt,
      -- Unofficial variable assignments, separated because of being
      -- global in the TeXbook.
      do
        (var, tgt) <- Par.parseXEqualsY (Par.headToParseSpecialInt t) Par.parseInt
        pure $ H.Syn.SpecialIntParameterAssignment var tgt,
      do
        (var, tgt) <- Par.parseXEqualsY (Par.headToParseSpecialLength t) Par.parseLength
        pure $ H.Syn.SpecialLengthParameterAssignment var tgt
    ]

parseTokenListTarget :: MonadPrimTokenSource m => m (H.Syn.TokenListAssignmentTarget 'H.Syn.Parsed)
parseTokenListTarget =
  PC.choice
    [ H.Syn.TokenListAssignmentText <$> Par.parseInhibitedGeneralText,
      do
        Par.skipFiller
        H.Syn.TokenListAssignmentVar <$> Par.parseHeaded Par.headToParseTokenListVariable
    ]

headToParseSetFamilyMember :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.AssignmentBody 'H.Syn.Parsed)
headToParseSetFamilyMember t = do
  (var, val) <- Par.parseXEqualsY (Par.headToParseFamilyMember t) (Par.parseHeaded Par.headToParseFontRef)
  pure $ H.Syn.SetFamilyMember var val

headToParseSetFontDimension :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.AssignmentBody 'H.Syn.Parsed)
headToParseSetFontDimension t = do
  (var, val) <- Par.parseXEqualsY (Par.headToParseFontDimensionRef t) Par.parseLength
  pure $ H.Syn.SetFontDimension var val

headToParseSetFontChar :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.AssignmentBody 'H.Syn.Parsed)
headToParseSetFontChar t = do
  (var, val) <- Par.parseXEqualsY (Par.headToParseFontCharRef t) Par.parseInt
  pure $ H.Syn.SetFontChar var val

headToParseSetBoxDimension :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.AssignmentBody 'H.Syn.Parsed)
headToParseSetBoxDimension t = do
  (var, val) <- Par.parseXEqualsY (Par.headToParseBoxDimensionRef t) Par.parseLength
  pure $ H.Syn.SetBoxDimension var val
