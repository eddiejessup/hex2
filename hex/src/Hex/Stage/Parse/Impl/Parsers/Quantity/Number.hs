module Hex.Stage.Parse.Impl.Parsers.Quantity.Number where

import Control.Monad.Combinators qualified as PC
import Data.ByteString qualified as BS
import Hex.Common.Ascii qualified as H.Ascii
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken (PrimitiveToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..), ParseUnexpectedErrorCause (..), UnexpectedPrimitiveToken (..), parseFailure)
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Lex.Interface.Extract (LexToken)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude
import qualified Hex.Common.Parse.Interface as Par
import qualified Formatting as F
import qualified Hex.Capability.Log.Interface as Log

parseSigned :: forall m a. MonadPrimTokenParse m => m a -> m (AST.Signed a)
parseSigned parseQuantity = AST.Signed <$> parseOptionalSigns <*> parseQuantity
  where
    parseOptionalSigns :: m [Q.Sign]
    parseOptionalSigns = do
      skipOptionalSpaces Expanding
      PC.sepEndBy (satisfyLexThen Expanding signToPos) (skipOptionalSpaces Expanding)
      where
        signToPos t
          | isOnly (lexTokenCatChar Code.Other) (Code.Chr_ '+') t = Just Q.Positive
          | isOnly (lexTokenCatChar Code.Other) (Code.Chr_ '-') t = Just Q.Negative
          | otherwise = Nothing

parseInt :: MonadPrimTokenParse m => m AST.HexInt
parseInt = AST.HexInt <$> parseSigned (anyPrim >>= headToParseUnsignedInt)

headToParseUnsignedInt :: MonadPrimTokenParse m => PrimitiveToken -> m AST.UnsignedInt
headToParseUnsignedInt headTok =
  choiceFlap
    [ fmap AST.NormalUnsignedInt <$> headToParseNormalInt,
      fmap AST.CoercedUnsignedInt <$> headToParseCoercedInt
    ]
    headTok

headToParseNormalInt :: forall m. MonadPrimTokenParse m => PrimitiveToken -> m AST.NormalInt
headToParseNormalInt headToken = do
  Log.log $ "headToParseNormalInt " <> F.sformat PT.fmtPrimitiveToken headToken
  choiceFlap
    [ \t -> liftLexHead headToParseConstantInt t <* skipOneOptionalSpace Expanding,
      fmap AST.InternalInt <$> headToParseInternalInt
    ]
    headToken
  where
    headToParseConstantInt :: LexToken -> m AST.NormalInt
    headToParseConstantInt t
      | Just w1 <- decCharToWord t = do
          remainingWs <- PC.many (satisfyLexThen Expanding decCharToWord)
          pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base10 (w1 : remainingWs)
      | isOnly (lexTokenCatChar Code.Other) (Code.Chr_ '"') t = do
          hexDigits <- PC.some (satisfyLexThen Expanding hexCharToWord)
          pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base16 hexDigits
      | isOnly (lexTokenCatChar Code.Other) (Code.Chr_ '\'') t = do
          octDigits <- PC.some (satisfyLexThen Expanding octCharToWord)
          pure $ AST.IntConstant $ AST.IntConstantDigits AST.Base8 octDigits
      | isOnly (lexTokenCatChar Code.Other) (Code.Chr_ '`') t =
          AST.CharLikeCode <$> Par.satisfyThenInhibited parseCharLikeCodeInt
      | otherwise =
          parseFailure $ "headToParseConstantInt " <> F.sformat Lex.fmtLexToken t

    -- Case 10, character constant like "`c".
    parseCharLikeCodeInt :: LexToken -> Maybe Word8
    parseCharLikeCodeInt = \case
      Lex.CharCatLexToken cc ->
        Just $ cc ^. typed @Code.CharCode % typed @Word8
      Lex.ControlSequenceLexToken cs -> do
        -- If bytestring is empty, fail to parse.
        case cs ^. typed @ByteString % to BS.uncons of
          Just (w, rest) ->
            -- Succeed if rest is empty, i.e. whole thing is one word long.
            if BS.null rest
              then Just w
              else Nothing
          Nothing ->
            Nothing

decCharToWord :: LexToken -> Maybe Word8
decCharToWord = fromCatChar Code.Other H.Ascii.fromDecDigit

hexCharToWord :: LexToken -> Maybe Word8
hexCharToWord lt = fromCatChar Code.Other H.Ascii.fromUpHexDigit lt <|> fromCatChar Code.Letter H.Ascii.fromUpHexAF lt

octCharToWord :: LexToken -> Maybe Word8
octCharToWord = fromCatChar Code.Other H.Ascii.fromOctDigit

fromCatChar :: Code.CoreCatCode -> (Word8 -> Maybe a) -> LexToken -> Maybe a
fromCatChar cat fromWord lt = (lt ^? lexTokenCatChar cat % typed @Word8) >>= fromWord

headToParseInternalInt :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.InternalInt
headToParseInternalInt =
  choiceFlap
    [ \case
        PT.LastPenaltyTok -> pure AST.LastPenalty
        PT.ParagraphShapeTok -> pure AST.ParShape
        PT.InputLineNrTok -> pure AST.InputLineNr
        PT.BadnessTok -> pure AST.Badness
        t -> parseFailure $ "headToParseInternalInt " <> F.sformat PT.fmtPrimitiveToken t,
      fmap AST.InternalIntVariable <$> headToParseIntVariable,
      fmap AST.InternalSpecialIntParameter <$> headToParseSpecialInt,
      fmap AST.InternalCodeTableRef <$> headToParseCodeTableRef,
      fmap AST.InternalCharToken <$> headToParseCharToken,
      fmap AST.InternalMathCharToken <$> headToParseMathCharToken,
      fmap AST.InternalFontSpecialCharRef <$> headToParseFontSpecialCharRef
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
  PT.ShortDefTargetToken (PT.ShortDefTargetValue PT.CharQuantity c) ->
    pure c
  t ->
    parseError $ SawUnexpectedPrimitiveToken $ UnexpectedPrimitiveToken {saw = t, expected = "ShortDefTargetValue CharQuantity"}

headToParseMathCharToken :: MonadPrimTokenParse m => PT.PrimitiveToken -> m Q.HexInt
headToParseMathCharToken = \case
  PT.ShortDefTargetToken (PT.ShortDefTargetValue PT.MathCharQuantity c) ->
    pure c
  t ->
    parseFailure $ "headToParseMathCharToken " <> F.sformat PT.fmtPrimitiveToken t

headToParseFontSpecialCharRef :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.FontSpecialCharRef
headToParseFontSpecialCharRef = \case
  PT.FontSpecialCharTok c ->
    AST.FontSpecialCharRef c <$> (anyPrim >>= headToParseFontRef)
  t ->
    parseFailure $ "headToParseFontSpecialCharRef " <> F.sformat PT.fmtPrimitiveToken t

headToParseFontRef :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.FontRef
headToParseFontRef =
  choiceFlap
    [ \case
        PT.FontTok -> pure AST.CurrentFontRef
        t -> parseFailure $ "headToParseFontRef " <> F.sformat PT.fmtPrimitiveToken t,
      fmap AST.FontTokenRef <$> headToParseFontRefToken,
      fmap AST.FamilyMemberFontRef <$> headToParseFamilyMember
    ]

headToParseFontRefToken :: MonadPrimTokenParse m => PT.PrimitiveToken -> m HSt.Font.FontNumber
headToParseFontRefToken = \case
  PT.FontRefToken n -> pure n
  t -> parseFailure $ "headToParseFontRefToken " <> F.sformat PT.fmtPrimitiveToken t

headToParseFamilyMember :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.FamilyMember
headToParseFamilyMember = \case
  PT.FontRangeTok r -> AST.FamilyMember r <$> parseInt
  t -> parseFailure $ "headToParseFamilyMember " <> F.sformat PT.fmtPrimitiveToken t

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
        t -> parseFailure $ "headToParseInternalLength " <> F.sformat PT.fmtPrimitiveToken t,
      fmap AST.InternalLengthVariable <$> headToParseLengthVariable,
      fmap AST.InternalSpecialLengthParameter <$> headToParseSpecialLength,
      fmap AST.InternalFontDimensionRef <$> headToParseFontDimensionRef,
      fmap AST.InternalBoxDimensionRef <$> headToParseBoxDimensionRef
    ]

headToParseFontDimensionRef :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.FontDimensionRef
headToParseFontDimensionRef = \case
  PT.FontDimensionTok ->
    AST.FontDimensionRef <$> parseInt <*> (anyPrim >>= headToParseFontRef)
  t ->
    parseFailure $ "headToParseFontDimensionRef " <> F.sformat PT.fmtPrimitiveToken t

headToParseBoxDimensionRef :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.BoxDimensionRef
headToParseBoxDimensionRef = \case
  PT.BoxDimensionTok dim -> do
    boxNr <- parseExplicitRegisterLocation
    pure $ AST.BoxDimensionRef boxNr dim
  t ->
    parseFailure $ "headToParseBoxDimensionRef " <> F.sformat PT.fmtPrimitiveToken t

-- Glue.

headToParseInternalGlue :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.InternalGlue
headToParseInternalGlue =
  choiceFlap
    [ fmap AST.InternalGlueVariable <$> headToParseGlueVariable,
      \case
        PT.LastGlueTok -> pure AST.LastGlue
        t -> parseFailure $ "headToParseInternalGlue " <> F.sformat PT.fmtPrimitiveToken t
    ]

-- Variables.
-- (Technically we could split some of these to other modules, but it seems
-- clearer to keep them together, because their structure is very similar.)
-- (Also, 'special' quantities are kept here too, because they are similar to
-- variables.)

parseExplicitRegisterLocation :: MonadPrimTokenParse m => m (AST.ExplicitRegisterLocation)
parseExplicitRegisterLocation = AST.ExplicitRegisterLocation <$> parseInt

headToParseIntVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.IntQuantity)
headToParseIntVariable = \case
  PT.IntParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.IntQuantParam p))
  PT.ShortDefTargetToken (PT.ShortDefTargetValue (PT.QuantityType Q.IntQuantity) n) ->
    pure $ AST.RegisterVar $ AST.InternalQuantRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.IntQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.IntQuantity ->
    AST.RegisterVar . AST.ExplicitQuantRegisterLocation HSt.Reg.IntQuantRegisterType <$> parseExplicitRegisterLocation
  t ->
    parseFailure $ "headToParseIntVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseLengthVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.LengthQuantity)
headToParseLengthVariable = \case
  PT.LengthParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.LengthQuantParam p))
  PT.ShortDefTargetToken (PT.ShortDefTargetValue (PT.QuantityType Q.LengthQuantity) n) ->
    pure $ AST.RegisterVar $ AST.InternalQuantRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.LengthQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.LengthQuantity ->
    AST.RegisterVar . AST.ExplicitQuantRegisterLocation HSt.Reg.LengthQuantRegisterType <$> parseExplicitRegisterLocation
  t ->
    parseFailure $ "headToParseLengthVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseGlueVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.GlueQuantity)
headToParseGlueVariable = \case
  PT.GlueParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.GlueQuantParam p))
  PT.ShortDefTargetToken (PT.ShortDefTargetValue (PT.QuantityType Q.GlueQuantity) n) ->
    pure $ AST.RegisterVar $ AST.InternalQuantRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.GlueQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.GlueQuantity ->
    AST.RegisterVar . AST.ExplicitQuantRegisterLocation HSt.Reg.GlueQuantRegisterType <$> parseExplicitRegisterLocation
  t ->
    parseFailure $ "headToParseGlueVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseMathGlueVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.MathGlueQuantity)
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
    parseFailure $ "headToParseMathGlueVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseTokenListVariable :: MonadPrimTokenParse m => PT.PrimitiveToken -> m (AST.QuantVariableAST 'Q.TokenListQuantity)
headToParseTokenListVariable = \case
  PT.TokenListParamVarTok p ->
    pure (AST.ParamVar (HSt.Param.TokenListQuantParam p))
  PT.ShortDefTargetToken (PT.ShortDefTargetValue (PT.QuantityType Q.TokenListQuantity) n) ->
    pure $ AST.RegisterVar $ AST.InternalQuantRegisterLocation (HSt.Reg.QuantRegisterLocation HSt.Reg.TokenListQuantRegisterType (HSt.Reg.RegisterLocation n))
  PT.RegisterVariableTok Q.TokenListQuantity ->
    AST.RegisterVar . AST.ExplicitQuantRegisterLocation HSt.Reg.TokenListQuantRegisterType <$> parseExplicitRegisterLocation
  t ->
    parseFailure $ "headToParseTokenListVariable " <> F.sformat PT.fmtPrimitiveToken t

headToParseSpecialInt :: MonadPrimTokenParse m => PT.PrimitiveToken -> m HSt.Param.SpecialIntParameter
headToParseSpecialInt = \case
  PT.SpecialIntParameterTok p ->
    pure p
  t ->
    parseFailure $ "headToParseSpecialInt " <> F.sformat PT.fmtPrimitiveToken t

headToParseSpecialLength :: MonadPrimTokenParse m => PT.PrimitiveToken -> m HSt.Param.SpecialLengthParameter
headToParseSpecialLength = \case
  PT.SpecialLengthParameterTok p ->
    pure p
  t ->
    parseFailure $ "headToParseSpecialLength " <> F.sformat PT.fmtPrimitiveToken t
