module Hex.Parse.Parsers.Quantity.Number where

import Control.Monad.Combinators qualified as PC
import Data.ByteString qualified as BS
import Hex.Ascii qualified as H.Ascii
import Hex.Codes qualified as H.C
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.Syntax.Quantity qualified as H.Par.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Syntax.Quantity qualified as H.Syn
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive (PrimitiveToken)
import Hex.Symbol.Token.Primitive qualified as T
import Hexlude

parseSigned :: forall m a. MonadPrimTokenSource m => m a -> m (H.Par.Syn.Signed a)
parseSigned parseQuantity = H.Par.Syn.Signed <$> parseOptionalSigns <*> parseQuantity
  where
    parseOptionalSigns :: m [H.Q.Sign]
    parseOptionalSigns = do
      skipOptionalSpaces
      PC.sepEndBy (satisfyThen signToPos) skipOptionalSpaces
      where
        signToPos t
          | isOnly (primTokCatChar H.C.Other) (H.C.Chr_ '+') t = Just H.Q.Positive
          | isOnly (primTokCatChar H.C.Other) (H.C.Chr_ '-') t = Just H.Q.Negative
          | otherwise = Nothing

parseInt :: MonadPrimTokenSource m => m H.Par.Syn.HexInt
parseInt = H.Par.Syn.HexInt <$> parseSigned (parseHeaded headToParseUnsignedInt)

headToParseUnsignedInt :: MonadPrimTokenSource m => PrimitiveToken -> m H.Par.Syn.UnsignedInt
headToParseUnsignedInt =
  choiceFlap
    [ fmap H.Par.Syn.NormalUnsignedInt <$> headToParseNormalInt,
      fmap H.Par.Syn.CoercedUnsignedInt <$> headToParseCoercedInt
    ]

headToParseNormalInt :: forall m. MonadPrimTokenSource m => PrimitiveToken -> m H.Par.Syn.NormalInt
headToParseNormalInt =
  choiceFlap
    [ \t -> headToParseConstantInt t <* skipOneOptionalSpace,
      fmap H.Par.Syn.InternalInt <$> headToParseInternalInt
    ]
  where
    headToParseConstantInt :: PrimitiveToken -> m H.Par.Syn.NormalInt
    headToParseConstantInt t
      | Just w1 <- decCharToWord t = do
        remainingWs <- PC.many (satisfyThen decCharToWord)
        pure $ H.Par.Syn.IntConstant $ H.Par.Syn.IntConstantDigits H.Par.Syn.Base10 (w1 : remainingWs)
      | isOnly (primTokCatChar H.C.Other) (H.C.Chr_ '"') t = do
        hexDigits <- PC.some (satisfyThen hexCharToWord)
        pure $ H.Par.Syn.IntConstant $ H.Par.Syn.IntConstantDigits H.Par.Syn.Base16 hexDigits
      | isOnly (primTokCatChar H.C.Other) (H.C.Chr_ '\'') t = do
        octDigits <- PC.some (satisfyThen octCharToWord)
        pure $ H.Par.Syn.IntConstant $ H.Par.Syn.IntConstantDigits H.Par.Syn.Base8 octDigits
      | isOnly (primTokCatChar H.C.Other) (H.C.Chr_ '`') t = do
        H.Par.Syn.CharLikeCode <$> parseCharLikeCodeInt
      | otherwise =
        empty

    -- Case 10, character constant like "`c".
    parseCharLikeCodeInt :: m Word8
    parseCharLikeCodeInt =
      fetchInhibitedLexToken >>= \case
        H.Lex.CharCatLexToken cc ->
          pure $ cc ^. typed @H.C.CharCode % typed @Word8
        H.Lex.ControlSequenceLexToken cs -> do
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
decCharToWord = fromCatChar H.C.Other H.Ascii.fromDecDigit

hexCharToWord :: PrimitiveToken -> Maybe Word8
hexCharToWord pt = fromCatChar H.C.Other H.Ascii.fromUpHexDigit pt <|> fromCatChar H.C.Letter H.Ascii.fromUpHexAF pt

octCharToWord :: PrimitiveToken -> Maybe Word8
octCharToWord = fromCatChar H.C.Other H.Ascii.fromOctDigit

fromCatChar :: H.C.CoreCatCode -> (Word8 -> Maybe a) -> PrimitiveToken -> Maybe a
fromCatChar cat fromWord pt = (pt ^? primTokCatChar cat % typed @Word8) >>= fromWord

headToParseInternalInt :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.InternalInt 'H.Syn.Parsed)
headToParseInternalInt =
  choiceFlap
    [ \case
        T.LastPenaltyTok -> pure H.Syn.LastPenalty
        T.ParagraphShapeTok -> pure H.Syn.ParShape
        T.InputLineNrTok -> pure H.Syn.InputLineNr
        T.BadnessTok -> pure H.Syn.Badness
        _ -> empty,
      fmap H.Syn.InternalIntVariable <$> headToParseIntVariable,
      fmap H.Syn.InternalSpecialIntParameter <$> headToParseSpecialInt,
      fmap H.Syn.InternalCodeTableRef <$> headToParseCodeTableRef,
      fmap H.Syn.InternalCharToken <$> headToParseCharToken,
      fmap H.Syn.InternalMathCharToken <$> headToParseMathCharToken,
      fmap H.Syn.InternalFontCharRef <$> headToParseFontCharRef
    ]

headToParseCoercedInt :: MonadPrimTokenSource m => PrimitiveToken -> m H.Par.Syn.CoercedInt
headToParseCoercedInt =
  choiceFlap
    [ fmap H.Par.Syn.InternalLengthAsInt <$> headToParseInternalLength,
      fmap H.Par.Syn.InternalGlueAsInt <$> headToParseInternalGlue
    ]

headToParseCodeTableRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.CodeTableRef 'H.Syn.Parsed)
headToParseCodeTableRef = \case
  T.CodeTypeTok c ->
    H.Syn.CodeTableRef c <$> parseInt
  t ->
    parseError $ SawUnexpectedToken $ UnexpectedToken {saw = t, expected = "CodeTypeTok"}

headToParseCharToken :: MonadPrimTokenSource m => T.PrimitiveToken -> m H.Q.HexInt
headToParseCharToken = \case
  T.IntRefTok T.CharQuantity c ->
    pure c
  t ->
    parseError $ SawUnexpectedToken $ UnexpectedToken {saw = t, expected = "IntRefTok CharQuantity"}

headToParseMathCharToken :: MonadPrimTokenSource m => T.PrimitiveToken -> m H.Q.HexInt
headToParseMathCharToken = \case
  T.IntRefTok T.MathCharQuantity c ->
    pure c
  _ ->
    empty

headToParseFontCharRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.FontCharRef 'H.Syn.Parsed)
headToParseFontCharRef = \case
  T.FontCharTok c ->
    H.Syn.FontCharRef c <$> parseHeaded headToParseFontRef
  _ ->
    empty

headToParseFontRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.FontRef 'H.Syn.Parsed)
headToParseFontRef =
  choiceFlap
    [ \case
        T.FontTok -> pure H.Syn.CurrentFontRef
        _ -> empty,
      fmap H.Syn.FontTokenRef <$> headToParseFontRefToken,
      fmap H.Syn.FamilyMemberFontRef <$> headToParseFamilyMember
    ]

headToParseFontRefToken :: MonadPrimTokenSource m => T.PrimitiveToken -> m T.FontNumber
headToParseFontRefToken = \case
  T.FontRefToken n -> pure n
  _ -> empty

headToParseFamilyMember :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.FamilyMember 'H.Syn.Parsed)
headToParseFamilyMember = \case
  T.FontRangeTok r -> H.Syn.FamilyMember r <$> parseInt
  _ -> empty

-- ======================
-- The remaining parsers aren't related to numbers, but because of coercion rules
-- (glue -> length, glue -> int, length -> int), we need them here to avoid circular dependencies.
-- ======================

-- Length.

headToParseInternalLength :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.InternalLength 'H.Syn.Parsed)
headToParseInternalLength =
  choiceFlap
    [ \case
        T.LastKernTok -> pure H.Syn.LastKern
        _ -> empty,
      fmap H.Syn.InternalLengthVariable <$> headToParseLengthVariable,
      fmap H.Syn.InternalSpecialLengthParameter <$> headToParseSpecialLength,
      fmap H.Syn.InternalFontDimensionRef <$> headToParseFontDimensionRef,
      fmap H.Syn.InternalBoxDimensionRef <$> headToParseBoxDimensionRef
    ]

headToParseFontDimensionRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.FontDimensionRef 'H.Syn.Parsed)
headToParseFontDimensionRef = \case
  T.FontDimensionTok ->
    H.Syn.FontDimensionRef <$> parseInt <*> parseHeaded headToParseFontRef
  _ ->
    empty

headToParseBoxDimensionRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.BoxDimensionRef 'H.Syn.Parsed)
headToParseBoxDimensionRef = \case
  T.BoxDimensionTok dim ->
    do
      boxNr <- parseInt
      pure $ H.Syn.BoxDimensionRef boxNr dim
  _ ->
    empty

-- Glue.

headToParseInternalGlue :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.InternalGlue 'H.Syn.Parsed)
headToParseInternalGlue =
  choiceFlap
    [ fmap H.Syn.InternalGlueVariable <$> headToParseGlueVariable,
      \case
        T.LastGlueTok -> pure H.Syn.LastGlue
        _ -> empty
    ]

-- Variables.
-- (Technically we could split some of these to other modules, but it seems
-- clearer to keep them together, because their structure is very similar.)
-- (Also, 'special' quantities are kept here too, because they are similar to
-- variables.)

headToParseIntVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.QuantVariable 'H.Syn.Parsed 'T.IntQuantity)
headToParseIntVariable = \case
  T.IntParamVarTok p ->
    pure (H.Syn.ParamVar p)
  T.IntRefTok (T.QuantityType T.IntQuantity) n ->
    pure $ H.Syn.RegisterVar $ H.Par.Syn.InternalRegisterIndex n
  T.RegisterVariableTok T.IntQuantity ->
    H.Syn.RegisterVar . H.Par.Syn.ExplicitRegisterIndex <$> parseInt
  _ ->
    empty

headToParseLengthVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.QuantVariable 'H.Syn.Parsed 'T.LengthQuantity)
headToParseLengthVariable = \case
  T.LengthParamVarTok p ->
    pure (H.Syn.ParamVar p)
  T.IntRefTok (T.QuantityType T.LengthQuantity) n ->
    pure $ H.Syn.RegisterVar $ H.Par.Syn.InternalRegisterIndex n
  T.RegisterVariableTok T.LengthQuantity ->
    H.Syn.RegisterVar . H.Par.Syn.ExplicitRegisterIndex <$> parseInt
  _ ->
    empty

headToParseGlueVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.QuantVariable 'H.Syn.Parsed 'T.GlueQuantity)
headToParseGlueVariable = \case
  T.GlueParamVarTok p ->
    pure (H.Syn.ParamVar p)
  T.IntRefTok (T.QuantityType T.GlueQuantity) n ->
    pure $ H.Syn.RegisterVar $ H.Par.Syn.InternalRegisterIndex n
  T.RegisterVariableTok T.GlueQuantity ->
    H.Syn.RegisterVar . H.Par.Syn.ExplicitRegisterIndex <$> parseInt
  _ ->
    empty

headToParseMathGlueVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.QuantVariable 'H.Syn.Parsed 'T.MathGlueQuantity)
headToParseMathGlueVariable = \case
  T.MathGlueParamVarTok p ->
    pure (H.Syn.ParamVar p)
  T.IntRefTok (T.QuantityType T.MathGlueQuantity) n ->
    pure $ H.Syn.RegisterVar $ H.Par.Syn.InternalRegisterIndex n
  T.RegisterVariableTok T.MathGlueQuantity ->
    H.Syn.RegisterVar . H.Par.Syn.ExplicitRegisterIndex <$> parseInt
  _ ->
    empty

headToParseTokenListVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (H.Syn.QuantVariable 'H.Syn.Parsed 'T.TokenListQuantity)
headToParseTokenListVariable = \case
  T.TokenListParamVarTok p ->
    pure (H.Syn.ParamVar p)
  T.IntRefTok (T.QuantityType T.TokenListQuantity) n ->
    pure $ H.Syn.RegisterVar $ H.Par.Syn.InternalRegisterIndex n
  T.RegisterVariableTok T.TokenListQuantity ->
    H.Syn.RegisterVar . H.Par.Syn.ExplicitRegisterIndex <$> parseInt
  _ ->
    empty

headToParseSpecialInt :: MonadPrimTokenSource m => T.PrimitiveToken -> m T.SpecialIntParameter
headToParseSpecialInt = \case
  T.SpecialIntParameterTok p ->
    pure p
  _ ->
    empty

headToParseSpecialLength :: MonadPrimTokenSource m => T.PrimitiveToken -> m T.SpecialLengthParameter
headToParseSpecialLength = \case
  T.SpecialLengthParameterTok p ->
    pure p
  _ ->
    empty
