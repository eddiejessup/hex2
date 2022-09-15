module Hex.Stage.Parse.Impl.Parsers.Quantity.Number where

import Control.Monad.Combinators qualified as PC
import Data.ByteString qualified as BS
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.Font qualified as Font
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive (PrimitiveToken)
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (ParseUnexpectedError (..), PrimTokenSource (..), UnexpectedPrimitiveToken (..), failParse, parseFail)
import Hex.Stage.Expand.Interface qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude

parseSigned :: forall es a. [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es a -> Eff es (AST.Signed a)
parseSigned parseQuantity = AST.Signed <$> parseOptionalSigns <*> parseQuantity
  where
    parseOptionalSigns :: Eff es [Q.Sign]
    parseOptionalSigns = do
      skipOptionalSpaces PT.Expanding
      PC.sepEndBy (satisfyCharCatThen PT.Expanding signToPos) (skipOptionalSpaces PT.Expanding)
      where
        signToPos cc
          | isOnly (ccCatChar Code.Other) (Code.Chr_ '+') cc = Just Q.Positive
          | isOnly (ccCatChar Code.Other) (Code.Chr_ '-') cc = Just Q.Negative
          | otherwise = Nothing

parseInt :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.HexInt
parseInt = AST.HexInt <$> parseSigned (anyPrim >>= headToParseUnsignedInt)

headToParseUnsignedInt :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PrimitiveToken -> Eff es AST.UnsignedInt
headToParseUnsignedInt headTok =
  choiceFlap
    [ fmap AST.NormalUnsignedInt <$> headToParseNormalInt,
      fmap AST.CoercedUnsignedInt <$> headToParseCoercedInt
    ]
    headTok

headToParseNormalInt :: forall es. [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PrimitiveToken -> Eff es AST.NormalInt
headToParseNormalInt headToken = do
  Log.debugLog $ "headToParseNormalInt " <> F.sformat PT.fmtPrimitiveToken headToken
  choiceFlap
    [ \t -> liftLexHead headToParseConstantInt t <* skipOneOptionalSpace PT.Expanding,
      fmap AST.InternalInt <$> headToParseInternalInt
    ]
    headToken
  where
    headToParseConstantInt :: LT.LexCharCat -> Eff es AST.NormalInt
    headToParseConstantInt cc
      | Just w1 <- decCharToWord cc = do
          remainingWs <- PC.many (satisfyCharCatThen PT.Expanding decCharToWord)
          pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base10 (w1 : remainingWs)
      | isOnly (ccCatChar Code.Other) (Code.Chr_ '"') cc = do
          hexDigits <- PC.some (satisfyCharCatThen PT.Expanding hexCharToWord)
          pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base16 hexDigits
      | isOnly (ccCatChar Code.Other) (Code.Chr_ '\'') cc = do
          octDigits <- PC.some (satisfyCharCatThen PT.Expanding octCharToWord)
          pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base8 octDigits
      | isOnly (ccCatChar Code.Other) (Code.Chr_ '`') cc =
          AST.CharLikeCode <$> Par.satisfyThenInhibited parseCharLikeCodeInt
      | otherwise =
          parseFail $ "headToParseConstantInt " <> F.sformat LT.fmtLexCharCat cc

    -- Case 10, character constant like "`c".
    parseCharLikeCodeInt :: LT.LexToken -> Maybe Word8
    parseCharLikeCodeInt = \case
      LT.CharCatLexToken cc ->
        Just $ cc ^. typed @Code.CharCode % typed @Word8
      LT.ControlSequenceLexToken cs -> do
        -- If bytestring is empty, fail to parse.
        case cs ^. typed @ByteString % to BS.uncons of
          Just (w, rest) ->
            -- Succeed if rest is empty, i.e. whole thing is one word long.
            if BS.null rest
              then Just w
              else Nothing
          Nothing ->
            Nothing

decCharToWord :: LT.LexCharCat -> Maybe Word8
decCharToWord = fromCatChar Code.Other H.Ascii.fromDecDigit

hexCharToWord :: LT.LexCharCat -> Maybe Word8
hexCharToWord lt = fromCatChar Code.Other H.Ascii.fromUpHexDigit lt <|> fromCatChar Code.Letter H.Ascii.fromUpHexAF lt

octCharToWord :: LT.LexCharCat -> Maybe Word8
octCharToWord = fromCatChar Code.Other H.Ascii.fromOctDigit

fromCatChar :: Code.CoreCatCode -> (Word8 -> Maybe a) -> LT.LexCharCat -> Maybe a
fromCatChar cat fromWord cc =
  if cat == cc.lexCCCat
    then fromWord cc.lexCCChar.unCharCode
    else Nothing

headToParseInternalInt :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.InternalInt
headToParseInternalInt =
  choiceFlap
    [ \case
        PT.LastPenaltyTok -> pure AST.LastPenalty
        PT.ParagraphShapeTok -> pure AST.ParShape
        PT.InputLineNrTok -> pure AST.InputLineNr
        PT.BadnessTok -> pure AST.Badness
        t -> parseFail $ "headToParseInternalInt " <> F.sformat PT.fmtPrimitiveToken t,
      fmap AST.InternalIntVariable <$> headToParseIntVariable,
      fmap AST.InternalSpecialIntParameter <$> headToParseSpecialInt,
      fmap AST.InternalCodeTableRef <$> headToParseCodeTableRef,
      fmap AST.InternalCharToken <$> headToParseCharToken,
      fmap AST.InternalMathCharToken <$> headToParseMathCharToken,
      fmap AST.InternalFontSpecialCharRef <$> headToParseFontSpecialCharRef
    ]

headToParseCoercedInt :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PrimitiveToken -> Eff es AST.CoercedInt
headToParseCoercedInt =
  choiceFlap
    [ fmap AST.InternalLengthAsInt <$> headToParseInternalLength,
      fmap AST.InternalGlueAsInt <$> headToParseInternalGlue
    ]

headToParseCodeTableRef :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.CodeTableRef
headToParseCodeTableRef = \case
  PT.CodeTypeTok c ->
    AST.CodeTableRef c <$> parseCharCodeInt
  t ->
    failParse $ SawUnexpectedPrimitiveToken $ UnexpectedPrimitiveToken {saw = t, expected = "CodeTypeTok"}

parseCharCodeInt :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.CharCodeInt
parseCharCodeInt = AST.CharCodeInt <$> parseInt

headToParseCharToken :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es Q.HexInt
headToParseCharToken = \case
  PT.ShortDefTargetToken (PT.ShortDefTargetValue PT.CharQuantity c) ->
    pure c
  t ->
    failParse $ SawUnexpectedPrimitiveToken $ UnexpectedPrimitiveToken {saw = t, expected = "ShortDefTargetValue CharQuantity"}

headToParseMathCharToken :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es Q.HexInt
headToParseMathCharToken = \case
  PT.ShortDefTargetToken (PT.ShortDefTargetValue PT.MathCharQuantity c) ->
    pure c
  t ->
    parseFail $ "headToParseMathCharToken " <> F.sformat PT.fmtPrimitiveToken t

headToParseFontSpecialCharRef :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.FontSpecialCharRef
headToParseFontSpecialCharRef = \case
  PT.FontSpecialCharTok c ->
    AST.FontSpecialCharRef c <$> (anyPrim >>= headToParseFontRef)
  t ->
    parseFail $ "headToParseFontSpecialCharRef " <> F.sformat PT.fmtPrimitiveToken t

headToParseFontRef :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.FontRef
headToParseFontRef =
  choiceFlap
    [ \case
        PT.FontTok -> pure AST.CurrentFontRef
        t -> parseFail $ "headToParseFontRef " <> F.sformat PT.fmtPrimitiveToken t,
      fmap AST.FontTokenRef <$> headToParseFontRefToken,
      fmap AST.FamilyMemberFontRef <$> headToParseFamilyMember
    ]

headToParseFontRefToken :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es Font.FontNumber
headToParseFontRefToken = \case
  PT.FontRefToken n -> pure n
  t -> parseFail $ "headToParseFontRefToken " <> F.sformat PT.fmtPrimitiveToken t

headToParseFamilyMember :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.FamilyMember
headToParseFamilyMember = \case
  PT.FontRangeTok r -> AST.FamilyMember r <$> parseInt
  t -> parseFail $ "headToParseFamilyMember " <> F.sformat PT.fmtPrimitiveToken t

-- ======================
-- The remaining parsers aren't related to numbers, but because of coercion rules
-- (glue -> length, glue -> int, length -> int), we need them here to avoid circular dependencies.
-- ======================

-- Length.

headToParseInternalLength :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.InternalLength
headToParseInternalLength =
  choiceFlap
    [ \case
        PT.LastKernTok -> pure AST.LastKern
        t -> parseFail $ "headToParseInternalLength " <> F.sformat PT.fmtPrimitiveToken t,
      fmap AST.InternalLengthVariable <$> headToParseLengthVariable,
      fmap AST.InternalSpecialLengthParameter <$> headToParseSpecialLength,
      fmap AST.InternalFontDimensionRef <$> headToParseFontDimensionRef,
      fmap AST.InternalBoxDimensionRef <$> headToParseBoxDimensionRef
    ]

headToParseFontDimensionRef :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.FontDimensionRef
headToParseFontDimensionRef = \case
  PT.FontDimensionTok ->
    AST.FontDimensionRef <$> parseInt <*> (anyPrim >>= headToParseFontRef)
  t ->
    parseFail $ "headToParseFontDimensionRef " <> F.sformat PT.fmtPrimitiveToken t

headToParseBoxDimensionRef :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.BoxDimensionRef
headToParseBoxDimensionRef = \case
  PT.BoxDimensionTok dim -> do
    boxNr <- parseExplicitRegisterLocation
    pure $ AST.BoxDimensionRef boxNr dim
  t ->
    parseFail $ "headToParseBoxDimensionRef " <> F.sformat PT.fmtPrimitiveToken t

-- Glue.

headToParseInternalGlue :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.InternalGlue
headToParseInternalGlue =
  choiceFlap
    [ fmap AST.InternalGlueVariable <$> headToParseGlueVariable,
      \case
        PT.LastGlueTok -> pure AST.LastGlue
        t -> parseFail $ "headToParseInternalGlue " <> F.sformat PT.fmtPrimitiveToken t
    ]

-- Variables.
-- (Technically we could split some of these to other modules, but it seems
-- clearer to keep them together, because their structure is very similar.)
-- (Also, 'special' quantities are kept here too, because they are similar to
-- variables.)

parseExplicitRegisterLocation :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es (AST.ExplicitRegisterLocation)
parseExplicitRegisterLocation = AST.ExplicitRegisterLocation <$> parseInt

headToParseIntVariable :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es (AST.QuantVariableAST 'Q.IntQuantity)
headToParseIntVariable = \case
  PT.IntParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.IntQuantParam p))
  PT.ShortDefTargetToken (PT.ShortDefTargetValue (PT.QuantityType Q.IntQuantity) n) ->
    pure $ AST.RegisterVar $ AST.InternalQuantRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.IntQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.IntQuantity ->
    AST.RegisterVar . AST.ExplicitQuantRegisterLocation HSt.Reg.IntQuantRegisterType <$> parseExplicitRegisterLocation
  t ->
    parseFail $ "headToParseIntVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseLengthVariable :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es (AST.QuantVariableAST 'Q.LengthQuantity)
headToParseLengthVariable = \case
  PT.LengthParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.LengthQuantParam p))
  PT.ShortDefTargetToken (PT.ShortDefTargetValue (PT.QuantityType Q.LengthQuantity) n) ->
    pure $ AST.RegisterVar $ AST.InternalQuantRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.LengthQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.LengthQuantity ->
    AST.RegisterVar . AST.ExplicitQuantRegisterLocation HSt.Reg.LengthQuantRegisterType <$> parseExplicitRegisterLocation
  t ->
    parseFail $ "headToParseLengthVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseGlueVariable :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es (AST.QuantVariableAST 'Q.GlueQuantity)
headToParseGlueVariable = \case
  PT.GlueParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.GlueQuantParam p))
  PT.ShortDefTargetToken (PT.ShortDefTargetValue (PT.QuantityType Q.GlueQuantity) n) ->
    pure $ AST.RegisterVar $ AST.InternalQuantRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.GlueQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.GlueQuantity ->
    AST.RegisterVar . AST.ExplicitQuantRegisterLocation HSt.Reg.GlueQuantRegisterType <$> parseExplicitRegisterLocation
  t ->
    parseFail $ "headToParseGlueVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseMathGlueVariable :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es (AST.QuantVariableAST 'Q.MathGlueQuantity)
headToParseMathGlueVariable = \case
  PT.MathGlueParamVarTok p ->
    pure $ AST.ParamVar $ HSt.Param.MathGlueQuantParam p
  PT.ShortDefTargetToken (PT.ShortDefTargetValue (PT.QuantityType Q.MathGlueQuantity) n) ->
    pure $
      AST.RegisterVar $
        AST.InternalQuantRegisterLocation $
          HSt.Reg.QuantRegisterLocation
            HSt.Reg.MathGlueQuantRegisterType
            (HSt.Reg.RegisterLocation n)
  PT.RegisterVariableTok Q.MathGlueQuantity ->
    AST.RegisterVar . AST.ExplicitQuantRegisterLocation HSt.Reg.MathGlueQuantRegisterType
      <$> parseExplicitRegisterLocation
  t ->
    parseFail $ "headToParseMathGlueVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseTokenListVariable :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es (AST.QuantVariableAST 'Q.TokenListQuantity)
headToParseTokenListVariable = \case
  PT.TokenListParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.TokenListQuantParam p))
  PT.ShortDefTargetToken (PT.ShortDefTargetValue (PT.QuantityType Q.TokenListQuantity) n) ->
    pure $ AST.RegisterVar $ AST.InternalQuantRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.TokenListQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.TokenListQuantity ->
    AST.RegisterVar . AST.ExplicitQuantRegisterLocation HSt.Reg.TokenListQuantRegisterType <$> parseExplicitRegisterLocation
  t ->
    parseFail $ "headToParseTokenListVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseSpecialInt :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es HSt.Param.SpecialIntParameter
headToParseSpecialInt = \case
  PT.SpecialIntParameterTok p ->
    pure p
  t ->
    parseFail $ "headToParseSpecialInt " <> F.sformat PT.fmtPrimitiveToken t

headToParseSpecialLength :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es HSt.Param.SpecialLengthParameter
headToParseSpecialLength = \case
  PT.SpecialLengthParameterTok p ->
    pure p
  t ->
    parseFail $ "headToParseSpecialLength " <> F.sformat PT.fmtPrimitiveToken t
