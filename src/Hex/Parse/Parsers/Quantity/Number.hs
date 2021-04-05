module Hex.Parse.Parsers.Quantity.Number where

import Control.Monad.Combinators qualified as PC
import Data.ByteString qualified as BS
import Hex.Ascii qualified as H.Ascii
import Hex.Codes qualified as H.C
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.AST qualified as AST
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Tokens (PrimitiveToken)
import Hex.Symbol.Tokens qualified as T
import Hexlude

parseSigned :: forall m a. MonadPrimTokenSource m => m a -> m (AST.Signed a)
parseSigned parseQuantity = AST.Signed <$> parseOptionalSigns <*> parseQuantity
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

parseInt :: MonadPrimTokenSource m => m AST.HexInt
parseInt = AST.HexInt <$> parseSigned (parseHeaded headToParseUnsignedInt)

headToParseUnsignedInt :: MonadPrimTokenSource m => PrimitiveToken -> m AST.UnsignedInt
headToParseUnsignedInt =
  choiceFlap
    [ fmap AST.NormalUnsignedInt <$> headToParseNormalInt,
      fmap AST.CoercedUnsignedInt <$> headToParseCoercedInt
    ]

headToParseNormalInt :: forall m. MonadPrimTokenSource m => PrimitiveToken -> m AST.NormalInt
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
      | isOnly (primTokCatChar H.C.Other) (H.C.Chr_ '"') t = do
        hexDigits <- PC.some (satisfyThen hexCharToWord)
        pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base16 hexDigits
      | isOnly (primTokCatChar H.C.Other) (H.C.Chr_ '\'') t = do
        octDigits <- PC.some (satisfyThen octCharToWord)
        pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base8 octDigits
      | isOnly (primTokCatChar H.C.Other) (H.C.Chr_ '`') t = do
        AST.CharLikeCode <$> parseCharLikeCodeInt
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

headToParseInternalInt :: MonadPrimTokenSource m => T.PrimitiveToken -> m AST.InternalInt
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

headToParseCoercedInt :: MonadPrimTokenSource m => PrimitiveToken -> m AST.CoercedInt
headToParseCoercedInt =
  choiceFlap
    [ fmap AST.InternalLengthAsInt <$> headToParseInternalLength,
      fmap AST.InternalGlueAsInt <$> headToParseInternalGlue
    ]

headToParseCodeTableRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m AST.CodeTableRef
headToParseCodeTableRef = \case
  T.CodeTypeTok c ->
    AST.CodeTableRef c <$> parseInt
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

headToParseFontCharRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m AST.FontCharRef
headToParseFontCharRef = \case
  T.FontCharTok c ->
    AST.FontCharRef c <$> parseHeaded headToParseFontRef
  _ ->
    empty

headToParseFontRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m AST.FontRef
headToParseFontRef =
  choiceFlap
    [ \case
        T.FontTok -> pure AST.CurrentFontRef
        _ -> empty,
      fmap AST.FontTokenRef <$> headToParseFontRefToken,
      fmap AST.FamilyMemberFontRef <$> headToParseFamilyMember
    ]

headToParseFontRefToken :: MonadPrimTokenSource m => T.PrimitiveToken -> m H.Q.HexInt
headToParseFontRefToken = \case
  T.FontRefToken n -> pure n
  _ -> empty

headToParseFamilyMember :: MonadPrimTokenSource m => T.PrimitiveToken -> m AST.FamilyMember
headToParseFamilyMember = \case
  T.FontRangeTok r -> AST.FamilyMember r <$> parseInt
  _ -> empty

-- ======================
-- The remaining parsers aren't related to numbers, but because of coercion rules
-- (glue -> length, glue -> int, length -> int), we need them here to avoid circular dependencies.
-- ======================

-- Length.

headToParseInternalLength :: MonadPrimTokenSource m => T.PrimitiveToken -> m AST.InternalLength
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

headToParseFontDimensionRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m AST.FontDimensionRef
headToParseFontDimensionRef = \case
  T.FontDimensionTok ->
    AST.FontDimensionRef <$> parseInt <*> parseHeaded headToParseFontRef
  _ ->
    empty

headToParseBoxDimensionRef :: MonadPrimTokenSource m => T.PrimitiveToken -> m AST.BoxDimensionRef
headToParseBoxDimensionRef = \case
  T.BoxDimensionTok dim ->
    do
      boxNr <- parseInt
      pure $ AST.BoxDimensionRef boxNr dim
  _ ->
    empty

-- Glue.

headToParseInternalGlue :: MonadPrimTokenSource m => T.PrimitiveToken -> m AST.InternalGlue
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

headToParseIntVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (AST.QuantVariableAST 'T.IntQuantity)
headToParseIntVariable = \case
  T.IntParamVarTok p ->
    pure (AST.ParamVar p)
  T.IntRefTok (T.QuantityType T.IntQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation n
  T.RegisterVariableTok T.IntQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation <$> parseInt
  _ ->
    empty

headToParseLengthVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (AST.QuantVariableAST 'T.LenQuantity)
headToParseLengthVariable = \case
  T.LenParamVarTok p ->
    pure (AST.ParamVar p)
  T.IntRefTok (T.QuantityType T.LenQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation n
  T.RegisterVariableTok T.LenQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation <$> parseInt
  _ ->
    empty

headToParseGlueVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (AST.QuantVariableAST 'T.GlueQuantity)
headToParseGlueVariable = \case
  T.GlueParamVarTok p ->
    pure (AST.ParamVar p)
  T.IntRefTok (T.QuantityType T.GlueQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation n
  T.RegisterVariableTok T.GlueQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation <$> parseInt
  _ ->
    empty

headToParseMathGlueVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (AST.QuantVariableAST 'T.MathGlueQuantity)
headToParseMathGlueVariable = \case
  T.MathGlueParamVarTok p ->
    pure (AST.ParamVar p)
  T.IntRefTok (T.QuantityType T.MathGlueQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation n
  T.RegisterVariableTok T.MathGlueQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation <$> parseInt
  _ ->
    empty

headToParseTokenListVariable :: MonadPrimTokenSource m => T.PrimitiveToken -> m (AST.QuantVariableAST 'T.TokenListQuantity)
headToParseTokenListVariable = \case
  T.TokenListParamVarTok p ->
    pure (AST.ParamVar p)
  T.IntRefTok (T.QuantityType T.TokenListQuantity) n ->
    pure $ AST.RegisterVar $ AST.InternalRegisterLocation n
  T.RegisterVariableTok T.TokenListQuantity ->
    AST.RegisterVar . AST.ExplicitRegisterLocation <$> parseInt
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
