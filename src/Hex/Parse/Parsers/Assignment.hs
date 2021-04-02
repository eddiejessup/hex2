module Hex.Parse.Parsers.Assignment where

import Control.Monad.Combinators qualified as PC
import Hex.Codes qualified as H.C
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.AST qualified as AST
import Hex.Symbol.Tokens qualified as T
import Hexlude
import qualified Hex.Quantity as H.Q
import Hex.Symbol.Tokens (PrimitiveToken)
import Hex.Parse.Parsers.Combinators
import Hex.Parse.Parsers.Quantity
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Ascii qualified as H.Ascii
import qualified ASCII

-- headToParseAssignment :: MonadPrimTokenSource m => PrimitiveToken -> m AST.Assignment
-- headToParseAssignment = go []
--   where
--     go prefixes = \case
--       T.AssignPrefixTok prefix ->
--         fetchPT >>= go (prefix : prefixes)
--       T.DefineMacroTok defGlobalType defExpandType -> do
--         -- Macro's name.
--         cs <- parseCSName
--         -- Parameter text.
--         (preParamTokens, parameters) <- parseParamText
--         when (defExpandType == T.ExpandDef) $ panic "Not implemented: ExpandDef"
--         -- Replacement text.
--         replacementTokens <- parseMacroText
--         let tgt = T.MacroContents
--               { T.preParamTokens = preParamTokens
--               , T.parameters = parameters
--               , T.replacementTokens = replacementTokens
--               , T.long = T.LongTok `elem` prefixes
--               , T.outer = T.OuterTok `elem` prefixes
--               }
--         let body = AST.DefineControlSequence cs (AST.MacroTarget tgt)
--         pure $ AST.Assignment
--           { body
--           , scope = if defGlobalType == T.Global || T.GlobalTok `elem` prefixes
--           then T.Global
--           else T.Local
--           }
--       t -> do
--         body <- headToParseNonMacroAssignmentBody t
--         pure $ AST.Assignment
--           { body
--           , scope = if T.GlobalTok `elem` prefixes
--           then T.Global
--           else T.Local
--           }

-- headToParseNonMacroAssignmentBody
--   :: MonadPrimTokenSource m
--   => PrimitiveToken
--   -> m AST.AssignmentBody
-- headToParseNonMacroAssignmentBody = \case
--   T.HyphenationTok -> AST.SetHyphenation <$> parseGeneralText
--   T.HyphenationPatternsTok ->
--     AST.SetHyphenationPatterns <$> parseGeneralText
--   T.InteractionModeTok intMode ->
--     pure (AST.SetInteractionMode intMode)
--   T.LetTok -> do
--     cs <- parseCSName
--     skipOptionalEquals
--     AST.DefineControlSequence cs <$> (AST.LetTarget <$> parseLetArg)
--   T.FutureLetTok ->
--     AST.DefineControlSequence <$>
--       parseCSName <*>
--       (AST.FutureLetTarget <$> parseLexToken <*> parseLexToken)
--   T.ShortDefHeadTok quant -> do
--     cs <- parseCSName
--     skipOptionalEquals
--     AST.DefineControlSequence cs <$> (AST.ShortDefineTarget quant <$> parseInt)
--   T.ParagraphShapeTok -> do
--     skipOptionalEquals
--     -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
--     -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
--     -- consecutive occurrences of ⟨dimen⟩
--     nrPairs <- parseInt
--     H.Q.HexInt eNrPairsInt <- texEvaluate nrPairs
--     let parseLengthPair = (,) <$> parseLength <*> parseLength
--     AST.SetParShape <$> PC.count eNrPairsInt parseLengthPair
--   T.ReadTok -> do
--     nr <- parseInt
--     skipKeyword (unsafeCodesFromChars "to")
--     skipOptionalSpaces
--     cs <- parseCSName
--     pure $ AST.DefineControlSequence cs (AST.ReadTarget nr)
--   T.SetBoxRegisterTok -> do
--     var <- parseEightBitInt
--     skipOptionalEquals
--     skipFiller
--     AST.SetBoxRegister var <$> parseHeaded headToParseBox
--   -- \font <control-sequence> <equals> <file-name> <at-clause>
--   T.FontTok -> do
--     cs <- parseCSName
--     skipOptionalEquals
--     fname <- parseFileName
--     fontSpec <-
--       PC.choice
--         [ skipKeyword (unsafeCodesFromChars "at") >> (AST.FontAt <$> parseLength)
--         , skipKeyword (unsafeCodesFromChars "scaled") >> (AST.FontScaled <$> parseInt)
--         , skipOptionalSpaces $> AST.NaturalFont
--         ]
--     pure $ AST.DefineControlSequence cs (AST.FontTarget fontSpec fname)
--   t ->
--     choiceFlap
--       [ headToParseCodeAssignment
--       , fmap AST.ModifyVariable <$> headToModifyVariable
--       , fmap AST.SetVariable <$> headToParseVariableAssignment
--       , fmap AST.SelectFont <$> headToParseFontRefToken
--       , headToParseSetFamilyMember
--       , headToParseSetFontDimension
--       , headToParseSetFontChar
--       , headToParseSetBoxDimension
--       ]
--       t
--   where
--     headToParseCodeAssignment t = do
--       ref <- headToParseCodeTableRef t
--       skipOptionalEquals
--       AST.AssignCode . AST.CodeAssignment ref <$> parseInt
--     headToModifyVariable = \case
--       T.AdvanceVarTok ->
--         PC.choice
--           [ do
--               var <- parseHeaded headToParseIntVariable
--               skipOptionalBy
--               AST.AdvanceIntVariable var <$> parseInt
--           , do
--             var <- parseHeaded headToParseLengthVariable
--             skipOptionalBy
--             AST.AdvanceLengthVariable var <$> parseLength
--           , do
--             var <- parseHeaded headToParseGlueVariable
--             skipOptionalBy
--             AST.AdvanceGlueVariable var <$> parseGlue
--           , do
--             var <- parseHeaded headToParseMathGlueVariable
--             skipOptionalBy
--             AST.AdvanceMathGlueVariable var <$> parseMathGlue
--           ]
--       T.ScaleVarTok d -> do
--         var <- parseNumericVariable
--         skipOptionalBy
--         AST.ScaleVariable d var <$> parseInt
--       t ->
--         parseError $ ParseErrorWithMsg $ "Expected 'AdvanceVarTok' or 'ScaleVarTok', saw " <> show t
--     parseNumericVariable =
--       PC.choice
--         [ IntNumericVariable <$> parseHeaded headToParseIntVariable
--         , LengthNumericVariable <$> parseHeaded headToParseLengthVariable
--         , GlueNumericVariable <$> parseHeaded headToParseGlueVariable
--         , MathGlueNumericVariable <$> parseHeaded headToParseMathGlueVariable
--         ]
--     skipOptionalBy =
--       PC.choice
--         [ void $ parseOptionalKeyword $ unsafeCodesFromChars "by"
--         , skipOptionalSpaces
--         ]
--     headToParseVariableAssignment t =
--       PC.choice
--         [ do
--             var <- headToParseIntVariable t
--             skipOptionalEquals
--             AST.IntVariableAssignment var <$> parseInt
--         , do
--           var <- headToParseLengthVariable t
--           skipOptionalEquals
--           AST.LengthVariableAssignment var <$> parseLength
--         , do
--           var <- headToParseGlueVariable t
--           skipOptionalEquals
--           AST.GlueVariableAssignment var <$> parseGlue
--         , do
--           var <- headToParseMathGlueVariable t
--           skipOptionalEquals
--           AST.MathGlueVariableAssignment var <$> parseMathGlue
--         , do
--           var <- headToParseTokenListVariable t
--           skipOptionalEquals
--           AST.TokenListVariableAssignment var <$> (TokenListAssignmentText <$> parseGeneralText)
--         , do
--           var <- headToParseTokenListVariable t
--           skipFiller
--           AST.TokenListVariableAssignment var <$> (TokenListAssignmentVar <$> parseHeaded headToParseTokenListVariable)
--         , -- Unofficial variable assignments, separated because of being
--         -- global in the TeXbook.
--         do
--           var <- headToParseSpecialInt t
--           skipOptionalEquals
--           AST.SpecialIntParameterVariableAssignment var <$> parseInt
--         , do
--           var <- headToParseSpecialLength t
--           skipOptionalEquals
--           AST.SpecialLengthParameterVariableAssignment var <$> parseLength
--         ]
--     headToParseSetFamilyMember t = do
--       var <- headToParseFamilyMember t
--       skipOptionalEquals
--       AST.SetFamilyMember var <$> parseHeaded headToParseFontRef
--     headToParseSetFontDimension t = do
--       var <- headToParseFontDimensionRef t
--       skipOptionalEquals
--       SetFontDimension var <$> parseLength
--     headToParseSetFontChar t = do
--       var <- headToParseFontCharRef t
--       skipOptionalEquals
--       AST.SetFontChar var <$> parseInt
--     headToParseSetBoxDimension t = do
--       var <- headToParseBoxDimensionRef t
--       skipOptionalEquals
--       AST.SetBoxDimension var <$> parseLength

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: MonadPrimTokenSource m => m AST.HexFilePath
parseFileName = do
  skipOptionalSpaces
  fileNameAsciiChars <-
    PC.some $ satisfyThen $ \pt -> do
      code <- pt ^? primTokCharCat >>= \case
        H.Lex.LexCharCat c H.C.Letter ->
          Just c
        H.Lex.LexCharCat c H.C.Other | isValidOther c ->
          Just c
        _ ->
          Nothing
      code ^. typed @Word8 % to ASCII.word8ToCharMaybe
  skipSatisfied isSpace
  pure $ AST.HexFilePath $ ASCII.charListToUnicodeString fileNameAsciiChars
  where
    isValidOther = \case
      -- Not in the spec, but let's say these are OK.
      H.C.CharCode_ '/' -> True
      H.C.CharCode_ '.' -> True
      H.C.CharCode_ '_' -> True
      -- 'Other' Characters for decimal digits are OK.
      cc ->
        H.Ascii.isDecDigit $ cc ^. typed @Word8
