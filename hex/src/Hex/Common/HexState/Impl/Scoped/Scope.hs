module Hex.Common.HexState.Impl.Scoped.Scope where

import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Impl.SymbolMap (initialSymbolMap)
import Hex.Common.HexState.Interface.Resolve (SymbolMap)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parameters qualified as Param
import Hex.Common.Quantity qualified as Q
import Hexlude

newtype RegisterLocation = RegisterLocation Q.HexInt
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

type CharCodeMap v = Map Codes.CharCode v
type RegisterMap v = Map RegisterLocation v

data Scope = Scope
  { -- Fonts.
    currentFontNr :: Maybe PT.FontNumber,
    -- familyMemberFonts :: Map (FontRange, HexInt) HexInt,
    -- Control sequences.
    symbolMap :: SymbolMap,
    -- Char-code attribute maps.
    catCodes :: CharCodeMap Codes.CatCode,
    mathCodes :: CharCodeMap Codes.MathCode,
    lowerCaseCodes :: CharCodeMap Codes.LowerCaseCode,
    upperCaseCodes :: CharCodeMap Codes.UpperCaseCode,
    spaceFactorCodes :: CharCodeMap Codes.SpaceFactorCode,
    delimiterCodes :: CharCodeMap Codes.DelimiterCode,
    -- Parameters.
    intParameters :: Map PT.IntParameter Q.HexInt,
    lengthParameters :: Map PT.LengthParameter Q.Length,
    glueParameters :: Map PT.GlueParameter Q.Glue,
    --   mathGlueParameters :: Map MathGlueParameter (BL.Glue MathLength),
    --   tokenListParameters :: Map TokenListParameter BalancedText
    -- Registers.
      intRegister :: RegisterMap Q.HexInt,
      lengthRegister :: RegisterMap Q.Length,
      glueRegister :: RegisterMap Q.Glue
    --   mathGlueRegister :: RegisterMap (BL.Glue MathLength),
    --   tokenListRegister :: RegisterMap BalancedText,
    --   boxRegister :: RegisterMap (B.Box B.BoxContents),
  }
  deriving stock (Show, Generic)

newGlobalScope :: Scope
newGlobalScope =
  Scope
    { currentFontNr = Nothing,
      -- familyMemberFonts = mempty
      symbolMap = initialSymbolMap,
      catCodes = Codes.newCatCodes,
      mathCodes = Codes.newMathCodes,
      lowerCaseCodes = Codes.newLowercaseCodes,
      upperCaseCodes = Codes.newUppercaseCodes,
      spaceFactorCodes = Codes.newspaceFactorCodes,
      delimiterCodes = Codes.newDelimiterCodes,
      intParameters = Param.newIntParameters,
      lengthParameters = Param.newLengthParameters,
      glueParameters = Param.newGlueParameters,
      -- , mathGlueParameters = newMathGlueParameters
      -- , tokenListParameters = newTokenListParameters
      intRegister = mempty,
      lengthRegister = mempty,
      glueRegister = mempty
      -- , mathGlueRegister = mempty
      -- , tokenListRegister = mempty
      -- , boxRegister = mempty
    }

newLocalScope :: Scope
newLocalScope =
  Scope
    { currentFontNr = Nothing,
      -- familyMemberFonts = mempty
      symbolMap = mempty,
      catCodes = mempty,
      mathCodes = mempty,
      lowerCaseCodes = mempty,
      upperCaseCodes = mempty,
      spaceFactorCodes = mempty,
      delimiterCodes = mempty,
      intParameters = mempty,
      lengthParameters = mempty,
      glueParameters = mempty,
      -- , mathGlueParameters = mempty
      -- , tokenListParameters = mempty
      intRegister = mempty,
      lengthRegister = mempty,
      glueRegister = mempty
      -- , mathGlueRegister = mempty
      -- , tokenListRegister = mempty
      -- , boxRegister = mempty
    }