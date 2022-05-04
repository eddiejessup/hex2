module Hex.Common.HexState.Impl.Scope where

import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Impl.Parameters qualified as H.Inter.St.Param
import Hex.Common.HexState.Impl.SymbolMap (initialSymbolMap)
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken, SymbolMap)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hexlude

data Scope = Scope
  { -- Fonts.
    currentFontNr :: Maybe PT.FontNumber,
    -- familyMemberFonts :: Map (FontRange, HexInt) HexInt,
    -- Control sequences.
    symbolMap :: SymbolMap,
    -- Char-code attribute maps.
    catCodes :: Map Codes.CharCode Codes.CatCode,
    mathCodes :: Map Codes.CharCode Codes.MathCode,
    lowerCaseCodes :: Map Codes.CharCode Codes.LowerCaseCode,
    upperCaseCodes :: Map Codes.CharCode Codes.UpperCaseCode,
    spaceFactorCodes :: Map Codes.CharCode Codes.SpaceFactorCode,
    delimiterCodes :: Map Codes.CharCode Codes.DelimiterCode,
    -- Parameters.
    intParameters :: Map PT.IntParameter Q.HexInt,
    lengthParameters :: Map PT.LengthParameter Q.Length,
    glueParameters :: Map PT.GlueParameter Q.Glue
    --   mathGlueParameters :: Map MathGlueParameter (BL.Glue MathLength),
    --   tokenListParameters :: Map TokenListParameter BalancedText
    -- Registers.
    --   hexIntRegister :: RegisterMap HexInt,
    --   lengthRegister :: RegisterMap Length,
    --   glueRegister :: RegisterMap (BL.Glue Length),
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
      intParameters = H.Inter.St.Param.newIntParameters,
      lengthParameters = H.Inter.St.Param.newLengthParameters,
      glueParameters = H.Inter.St.Param.newGlueParameters
      -- , mathGlueParameters = newMathGlueParameters
      -- , tokenListParameters = newTokenListParameters
      -- , hexIntRegister = mempty
      -- , lengthRegister = mempty
      -- , glueRegister = mempty
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
      glueParameters = mempty
      -- , mathGlueParameters = mempty
      -- , tokenListParameters = mempty
      -- , hexIntRegister = mempty
      -- , lengthRegister = mempty
      -- , glueRegister = mempty
      -- , mathGlueRegister = mempty
      -- , tokenListRegister = mempty
      -- , boxRegister = mempty
    }

-- In a single scope...

-- | The token a control-symbol resolves to
scopeResolvedTokenLens :: ControlSymbol -> Lens' Scope (Maybe ResolvedToken)
scopeResolvedTokenLens p = #symbolMap % at' p

-- The value for an integer-parameter.
scopeIntParamLens :: PT.IntParameter -> Lens' Scope (Maybe Q.HexInt)
scopeIntParamLens p = #intParameters % at' p

-- The value for a length-parameter.
scopeLengthParamLens :: PT.LengthParameter -> Lens' Scope (Maybe Q.Length)
scopeLengthParamLens p = #lengthParameters % at' p

-- The value for a glue-parameter.
scopeGlueParamLens :: PT.GlueParameter -> Lens' Scope (Maybe Q.Glue)
scopeGlueParamLens p = #glueParameters % at' p
