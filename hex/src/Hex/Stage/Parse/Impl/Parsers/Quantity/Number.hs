module Hex.Stage.Parse.Impl.Parsers.Quantity.Number where

import Control.Monad.Combinators qualified as PC
import Data.ByteString qualified as BS
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..), ParseUnexpectedErrorCause (..), UnexpectedPrimitiveToken (..), parseFailure)
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude

parseSigned :: forall m a. MonadPrimTokenParse m => m a -> m (AST.Signed a)
parseSigned parseQuantity = AST.Signed <$> parseOptionalSigns <*> parseQuantity
  where
    parseOptionalSigns :: m [Q.Sign]
    parseOptionalSigns = do
      skipOptionalSpaces
      PC.sepEndBy (satisfyThen signToPos) skipOptionalSpaces
      where
        signToPos t
          | isOnly (primTokCatChar Code.Other) (Code.Chr_ '+') t = Just Q.Positive
          | isOnly (primTokCatChar Code.Other) (Code.Chr_ '-') t = Just Q.Negative
          | otherwise = Nothing

parseInt :: MonadPrimTokenParse m => m AST.HexInt
parseInt = AST.HexInt <$> parseSigned (parseHeaded headToParseUnsignedInt)

headToParseUnsignedInt :: MonadPrimTokenParse m => PrimitiveToken -> m AST.UnsignedInt
headToParseUnsignedInt headTok =
  choiceFlap
    [ fmap AST.NormalUnsignedInt <$> headToParseNormalInt,
      fmap AST.CoercedUnsignedInt <$> headToParseCoercedInt
    ]
    headTok

headToParseNormalInt :: forall m. MonadPrimTokenParse m => PrimitiveToken -> m AST.NormalInt
headToParseNormalInt =
  choiceFlap
    [ \t -> headToParseConstantInt t <* skipOneOptionalSpace,
      fmap AST.InternalInt <$> headToParseInternalInt
    ]
  where
    headToParseConstantInt :: PrimitiveToken -> m AST.NormalInt
    headToParseConstantInt t
      | Just w1 <- decCharToWord t = do
          remainingWs <- PC.many (satisfyThen decCharToWord)
          pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base10 (w1 : remainingWs)
      | isOnly (primTokCatChar Code.Other) (Code.Chr_ '"') t = do
          hexDigits <- PC.some (satisfyThen hexCharToWord)
          pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base16 hexDigits
      | isOnly (primTokCatChar Code.Other) (Code.Chr_ '\'') t = do
          octDigits <- PC.some (satisfyThen octCharToWord)
          pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base8 octDigits
      | isOnly (primTokCatChar Code.Other) (Code.Chr_ '`') t =
          AST.CharLikeCode <$> parseCharLikeCodeInt
      | otherwise =
          parseFailure "headToParseConstantInt"

    -- Case 10, character constant like "`c".
    parseCharLikeCodeInt :: m Word8
    parseCharLikeCodeInt =
      getUnexpandedToken >>= \case
        Lex.CharCatLexToken cc ->
          pure $ cc ^. typed @Code.CharCode % typed @Word8
        Lex.ControlSequenceLexToken cs -> do
          -- If bytestring is empty, fail to parse.
          case cs ^. typed @ByteString % to BS.uncons of
            Just (w, rest) ->
              -- Succeed if rest is empty, i.e. whole thing is one word long.
              if BS.null rest
                then pure w
                else parseFailure "parseCharLikeCodeInt"
            Nothing ->
              parseFailure "parseCharLikeCodeInt"

decCharToWord :: PrimitiveToken -> Maybe Word8
decCharToWord = fromCatChar Code.Other H.Ascii.fromDecDigit

hexCharToWord :: PrimitiveToken -> Maybe Word8
hexCharToWord pt = fromCatChar Code.Other H.Ascii.fromUpHexDigit pt <|> fromCatChar Code.Letter H.Ascii.fromUpHexAF pt

octCharToWord :: PrimitiveToken -> Maybe Word8
octCharToWord = fromCatChar Code.Other H.Ascii.fromOctDigit

fromCatChar :: Code.CoreCatCode -> (Word8 -> Maybe a) -> PrimitiveToken -> Maybe a
fromCatChar cat fromWord pt = (pt ^? primTokCatChar cat % typed @Word8) >>= fromWord

headToParseInternalInt :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.InternalInt
headToParseInternalInt =
  choiceFlap
    [ \case
        PT.LastPenaltyTok -> pure AST.LastPenalty
        PT.ParagraphShapeTok -> pure AST.ParShape
        PT.InputLineNrTok -> pure AST.InputLineNr
        PT.BadnessTok -> pure AST.Badness
        _ -> parseFailure "headToParseInternalInt",
      fmap AST.InternalIntVariable <$> headToParseIntVariable,
      fmap AST.InternalSpecialIntParameter <$> headToParseSpecialInt,
      fmap AST.InternalCodeTableRef <$> headToParseCodeTableRef,
      fmap AST.InternalCharToken <$> headToParseCharToken,
      fmap AST.InternalMathCharToken <$> headToParseMathCharToken,
      fmap AST.InternalFontCharRef <$> headToParseFontCharRef
    ]

headToParseCoercedInt :: MonadPrimTokenParse m => PrimitiveToken -> m AST.CoercedInt
headToParseCoercedInt =
  choiceFlap
    [ fmap AST.InternalLengthAsInt <$> headToParseInternalLength,
      fmap AST.InternalGlueAsInt <$> headToParseInternalGlue
    ]

headToParseCodeTableRef :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.CodeTableRef
headToParseCodeTableRef = \case
  PT.CodeTypeTok c ->
    AST.CodeTableRef c <$> parseCharCodeInt
  t ->
    parseError $ SawUnexpectedPrimitiveToken $ UnexpectedPrimitiveToken {saw = t, expected = "CodeTypeTok"}

parseCharCodeInt :: MonadPrimTokenParse m => m AST.CharCodeInt
parseCharCodeInt = AST.CharCodeInt <$> parseInt

headToParseCharToken :: MonadPrimTokenParse m => PT.PrimitiveToken -> m Q.HexInt
headToParseCharToken = \case
  PT.IntRefTok PT.CharQuantity c ->
    pure c
  t ->
    parseError $ SawUnexpectedPrimitiveToken $ UnexpectedPrimitiveToken {saw = t, expected = "IntRefTok CharQuantity"}

headToParseMathCharToken :: MonadPrimTokenParse m => PT.PrimitiveToken -> m Q.HexInt
headToParseMathCharToken = \case
  PT.IntRefTok PT.MathCharQuantity c ->
    pure c
  _ ->
    parseFailure "headToParseMathCharToken"

headToParseFontCharRef :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.FontCharRef
headToParseFontCharRef = \case
  PT.FontCharTok c ->
    AST.FontCharRef c <$> parseHeaded headToParseFontRef
  _ ->
    parseFailure "headToParseFontCharRef"

headToParseFontRef :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.FontRef
headToParseFontRef =
  choiceFlap
    [ \case
        PT.FontTok -> pure AST.CurrentFontRef
        _ -> parseFailure "headToParseFontRef",
      fmap AST.FontTokenRef <$> headToParseFontRefToken,
      fmap AST.FamilyMemberFontRef <$> headToParseFamilyMember
    ]

headToParseFontRefToken :: MonadPrimTokenParse m => PT.PrimitiveToken -> m PT.FontNumber
headToParseFontRefToken = \case
  PT.FontRefToken n -> pure n
  _ -> parseFailure "headToParseFontRefToken"

headToParseFamilyMember :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.FamilyMember
headToParseFamilyMember = \case
  PT.FontRangeTok r -> AST.FamilyMember r <$> parseInt
  _ -> parseFailure "headToParseFamilyMember"

-- ======================
-- The remaining parsers aren't related to numbers, but because of coercion rules
-- (glue -> length, glue -> int, length -> int), we need them here to avoid circular dependencies.
-- ======================

-- Length.

headToParseInternalLength :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.InternalLength
headToParseInternalLength =
  choiceFlap
    [ \case
        PT.LastKernTok -> pure AST.LastKern
        _ -> parseFailure "headToParseInternalLength",
      fmap AST.InternalLengthVariable <$> headToParseLengthVariable,
      fmap AST.InternalSpecialLengthParameter <$> headToParseSpecialLength,
      fmap AST.InternalFontDimensionRef <$> headToParseFontDimensionRef,
      fmap AST.InternalBoxDimensionRef <$> headToParseBoxDimensionRef
    ]

headToParseFontDimensionRef :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.FontDimensionRef
headToParseFontDimensionRef = \case
  PT.FontDimensionTok ->
    AST.FontDimensionRef <$> parseInt <*> parseHeaded headToParseFontRef
  _ ->
    parseFailure "headToParseFontDimensionRef"

headToParseBoxDimensionRef :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.BoxDimensionRef
headToParseBoxDimensionRef = \case
  PT.BoxDimensionTok dim -> do
    boxNr <- parseInt
    pure $ AST.BoxDimensionRef boxNr dim
  _ ->
    parseFailure "headToParseBoxDimensionRef"

-- Glue.

headToParseInternalGlue :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.InternalGlue
headToParseInternalGlue =
  choiceFlap
    [ fmap AST.InternalGlueVariable <$> headToParseGlueVariable,
      \case
        PT.LastGlueTok -> pure AST.LastGlue
        _ -> parseFailure "headToParseInternalGlue"
    ]

-- Variables.
-- (Technically we could split some of these to other modules, but it seems
-- clearer to keep them together, because their structure is very similar.)
-- (Also, 'special' quantities are kept here too, because they are similar to
-- variables.)

headToParseIntVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.IntQuantity)
headToParseIntVariable = \case
  PT.IntParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.IntQuantParam p))
  PT.IntRefTok (PT.QuantityType Q.IntQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.IntQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.IntQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.IntQuantRegisterType <$> parseInt
  _ ->
    parseFailure "headToParseIntVariable"

headToParseLengthVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.LengthQuantity)
headToParseLengthVariable = \case
  PT.LengthParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.LengthQuantParam p))
  PT.IntRefTok (PT.QuantityType Q.LengthQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.LengthQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.LengthQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.LengthQuantRegisterType <$> parseInt
  _ ->
    parseFailure "headToParseLengthVariable"

headToParseGlueVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.GlueQuantity)
headToParseGlueVariable = \case
  PT.GlueParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.GlueQuantParam p))
  PT.IntRefTok (PT.QuantityType Q.GlueQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.GlueQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.GlueQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.GlueQuantRegisterType <$> parseInt
  _ ->
    parseFailure "headToParseGlueVariable"

headToParseMathGlueVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.MathGlueQuantity)
headToParseMathGlueVariable = \case
  PT.MathGlueParamVarTok p ->
    pure $ AST.ParamVar $ HSt.Param.MathGlueQuantParam p
  PT.IntRefTok (PT.QuantityType Q.MathGlueQuantity) n ->
    pure $
      AST.RegisterVar $
        AST.InternalRegisterLocation $
          HSt.Reg.QuantRegisterLocation
            HSt.Reg.MathGlueQuantRegisterType
            (HSt.Reg.RegisterLocation n)
  PT.RegisterVariableTok Q.MathGlueQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.MathGlueQuantRegisterType
      <$> parseInt
  _ ->
    parseFailure "headToParseMathGlueVariable"

headToParseTokenListVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.TokenListQuantity)
headToParseTokenListVariable = \case
  PT.TokenListParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.TokenListQuantParam p))
  PT.IntRefTok (PT.QuantityType Q.TokenListQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.TokenListQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.TokenListQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.TokenListQuantRegisterType <$> parseInt
  _ ->
    parseFailure "headToParseTokenListVariable"

headToParseSpecialInt :: MonadPrimTokenParse m => PT.PrimitiveToken -> m HSt.Param.SpecialIntParameter
headToParseSpecialInt = \case
  PT.SpecialIntParameterTok p ->
    pure p
  _ ->
    parseFailure "headToParseSpecialInt"

headToParseSpecialLength :: MonadPrimTokenParse m => PT.PrimitiveToken -> m HSt.Param.SpecialLengthParameter
headToParseSpecialLength = \case
  PT.SpecialLengthParameterTok p ->
    pure p
  _ ->
    parseFailure "headToParseSpecialLength"
