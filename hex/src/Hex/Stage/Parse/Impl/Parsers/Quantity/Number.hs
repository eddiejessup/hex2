module Hex.Stage.Parse.Impl.Parsers.Quantity.Number where

import Control.Monad.Combinators qualified as PC
import Data.ByteString qualified as BS
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..), ParseUnexpectedErrorCause (..), UnexpectedPrimitiveToken (..))
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
          empty

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
                else empty
            Nothing ->
              empty

decCharToWord :: PrimitiveToken -> Maybe Word8
decCharToWord = fromCatChar Code.Other H.Ascii.fromDecDigit

hexCharToWord :: PrimitiveToken -> Maybe Word8
hexCharToWord pt = fromCatChar Code.Other H.Ascii.fromUpHexDigit pt <|> fromCatChar Code.Letter H.Ascii.fromUpHexAF pt

octCharToWord :: PrimitiveToken -> Maybe Word8
octCharToWord = fromCatChar Code.Other H.Ascii.fromOctDigit

fromCatChar :: Code.CoreCatCode -> (Word8 -> Maybe a) -> PrimitiveToken -> Maybe a
fromCatChar cat fromWord pt = (pt ^? primTokCatChar cat % typed @Word8) >>= fromWord

headToParseInternalInt :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.InternalInt
headToParseInternalInt =
  choiceFlap
    [ \case
        T.LastPenaltyTok -> pure AST.LastPenalty
        T.ParagraphShapeTok -> pure AST.ParShape
        T.InputLineNrTok -> pure AST.InputLineNr
        T.BadnessTok -> pure AST.Badness
        _ -> empty,
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

headToParseCodeTableRef :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.CodeTableRef
headToParseCodeTableRef = \case
  T.CodeTypeTok c ->
    AST.CodeTableRef c <$> parseCharCodeInt
  t ->
    parseError $ SawUnexpectedPrimitiveToken $ UnexpectedPrimitiveToken {saw = t, expected = "CodeTypeTok"}

parseCharCodeInt :: MonadPrimTokenParse m => m AST.CharCodeInt
parseCharCodeInt = AST.CharCodeInt <$> parseInt

headToParseCharToken :: MonadPrimTokenParse m => T.PrimitiveToken -> m Q.HexInt
headToParseCharToken = \case
  T.IntRefTok T.CharQuantity c ->
    pure c
  t ->
    parseError $ SawUnexpectedPrimitiveToken $ UnexpectedPrimitiveToken {saw = t, expected = "IntRefTok CharQuantity"}

headToParseMathCharToken :: MonadPrimTokenParse m => T.PrimitiveToken -> m Q.HexInt
headToParseMathCharToken = \case
  T.IntRefTok T.MathCharQuantity c ->
    pure c
  _ ->
    empty

headToParseFontCharRef :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.FontCharRef
headToParseFontCharRef = \case
  T.FontCharTok c ->
    AST.FontCharRef c <$> parseHeaded headToParseFontRef
  _ ->
    empty

headToParseFontRef :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.FontRef
headToParseFontRef =
  choiceFlap
    [ \case
        T.FontTok -> pure AST.CurrentFontRef
        _ -> empty,
      fmap AST.FontTokenRef <$> headToParseFontRefToken,
      fmap AST.FamilyMemberFontRef <$> headToParseFamilyMember
    ]

headToParseFontRefToken :: MonadPrimTokenParse m => T.PrimitiveToken -> m T.FontNumber
headToParseFontRefToken = \case
  T.FontRefToken n -> pure n
  _ -> empty

headToParseFamilyMember :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.FamilyMember
headToParseFamilyMember = \case
  T.FontRangeTok r -> AST.FamilyMember r <$> parseInt
  _ -> empty

-- ======================
-- The remaining parsers aren't related to numbers, but because of coercion rules
-- (glue -> length, glue -> int, length -> int), we need them here to avoid circular dependencies.
-- ======================

-- Length.

headToParseInternalLength :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.InternalLength
headToParseInternalLength =
  choiceFlap
    [ \case
        T.LastKernTok -> pure AST.LastKern
        _ -> empty,
      fmap AST.InternalLengthVariable <$> headToParseLengthVariable,
      fmap AST.InternalSpecialLengthParameter <$> headToParseSpecialLength,
      fmap AST.InternalFontDimensionRef <$> headToParseFontDimensionRef,
      fmap AST.InternalBoxDimensionRef <$> headToParseBoxDimensionRef
    ]

headToParseFontDimensionRef :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.FontDimensionRef
headToParseFontDimensionRef = \case
  T.FontDimensionTok ->
    AST.FontDimensionRef <$> parseInt <*> parseHeaded headToParseFontRef
  _ ->
    empty

headToParseBoxDimensionRef :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.BoxDimensionRef
headToParseBoxDimensionRef = \case
  T.BoxDimensionTok dim ->
    do
      boxNr <- parseInt
      pure $ AST.BoxDimensionRef boxNr dim
  _ ->
    empty

-- Glue.

headToParseInternalGlue :: MonadPrimTokenParse m => T.PrimitiveToken -> m AST.InternalGlue
headToParseInternalGlue =
  choiceFlap
    [ fmap AST.InternalGlueVariable <$> headToParseGlueVariable,
      \case
        T.LastGlueTok -> pure AST.LastGlue
        _ -> empty
    ]

-- Variables.
-- (Technically we could split some of these to other modules, but it seems
-- clearer to keep them together, because their structure is very similar.)
-- (Also, 'special' quantities are kept here too, because they are similar to
-- variables.)

headToParseIntVariable :: MonadPrimTokenParse m => T.PrimitiveToken -> m (AST.QuantVariableAST 'Q.IntQuantity)
headToParseIntVariable = \case
  T.IntParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.IntQuantParam p))
  T.IntRefTok (T.QuantityType Q.IntQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.IntQuantRegisterType (HSt.Reg.RegisterLocation n))
  T.RegisterVariableTok Q.IntQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.IntQuantRegisterType <$> parseInt
  _ ->
    empty

headToParseLengthVariable :: MonadPrimTokenParse m => T.PrimitiveToken -> m (AST.QuantVariableAST 'Q.LengthQuantity)
headToParseLengthVariable = \case
  T.LengthParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.LengthQuantParam p))
  T.IntRefTok (T.QuantityType Q.LengthQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.LengthQuantRegisterType (HSt.Reg.RegisterLocation n))
  T.RegisterVariableTok Q.LengthQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.LengthQuantRegisterType <$> parseInt
  _ ->
    empty

headToParseGlueVariable :: MonadPrimTokenParse m => T.PrimitiveToken -> m (AST.QuantVariableAST 'Q.GlueQuantity)
headToParseGlueVariable = \case
  T.GlueParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.GlueQuantParam p))
  T.IntRefTok (T.QuantityType Q.GlueQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.GlueQuantRegisterType (HSt.Reg.RegisterLocation n))
  T.RegisterVariableTok Q.GlueQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.GlueQuantRegisterType <$> parseInt
  _ ->
    empty

headToParseMathGlueVariable :: MonadPrimTokenParse m => T.PrimitiveToken -> m (AST.QuantVariableAST 'Q.MathGlueQuantity)
headToParseMathGlueVariable = \case
  T.MathGlueParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.MathGlueQuantParam p))
  T.IntRefTok (T.QuantityType Q.MathGlueQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.MathGlueQuantRegisterType (HSt.Reg.RegisterLocation n))
  T.RegisterVariableTok Q.MathGlueQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.MathGlueQuantRegisterType <$> parseInt
  _ ->
    empty

headToParseTokenListVariable :: MonadPrimTokenParse m => T.PrimitiveToken -> m (AST.QuantVariableAST 'Q.TokenListQuantity)
headToParseTokenListVariable = \case
  T.TokenListParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.TokenListQuantParam p))
  T.IntRefTok (T.QuantityType Q.TokenListQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.TokenListQuantRegisterType (HSt.Reg.RegisterLocation n))
  T.RegisterVariableTok Q.TokenListQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation HSt.Reg.TokenListQuantRegisterType <$> parseInt
  _ ->
    empty

headToParseSpecialInt :: MonadPrimTokenParse m => T.PrimitiveToken -> m HSt.Param.SpecialIntParameter
headToParseSpecialInt = \case
  T.SpecialIntParameterTok p ->
    pure p
  _ ->
    empty

headToParseSpecialLength :: MonadPrimTokenParse m => T.PrimitiveToken -> m HSt.Param.SpecialLengthParameter
headToParseSpecialLength = \case
  T.SpecialLengthParameterTok p ->
    pure p
  _ ->
    empty
