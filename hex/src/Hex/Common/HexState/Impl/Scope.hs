module Hex.Common.HexState.Impl.Scope where

import ASCII qualified
import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Impl.Parameters qualified as H.Inter.St.Param
import Hex.Common.Quantity qualified as H.Q
import Hexlude
import qualified Hex.Common.HexState.Interface.Resolve.PrimitiveToken as PT
import Hex.Common.HexState.Interface.Resolve (CSMap, ResolvedToken, ControlSymbol)
import Hex.Common.HexState.Impl.CSMap (initialCSMap)


data Scope = Scope
  { -- Fonts.
    currentFontNr :: Maybe PT.FontNumber,
    -- familyMemberFonts :: Map (FontRange, HexInt) HexInt,
    -- Control sequences.
    csMap :: CSMap,
    -- Char-code attribute maps.
    catCodes :: Map Codes.CharCode Codes.CatCode,
    mathCodes :: Map Codes.CharCode Codes.MathCode,
    lowercaseCodes :: Map Codes.CharCode Codes.CaseChangeCode,
    uppercaseCodes :: Map Codes.CharCode Codes.CaseChangeCode,
    spaceFactors :: Map Codes.CharCode Codes.SpaceFactorCode,
    delimiterCodes :: Map Codes.CharCode Codes.DelimiterCode,
    -- Parameters.
    intParameters :: Map PT.IntParameter H.Q.HexInt,
    lengthParameters :: Map PT.LengthParameter H.Q.Length,
    glueParameters :: Map PT.GlueParameter H.Q.Glue
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
      csMap = initialCSMap,
      catCodes = Codes.newCatCodes,
      mathCodes = Codes.newMathCodes,
      lowercaseCodes = Codes.newLowercaseCodes,
      uppercaseCodes = Codes.newUppercaseCodes,
      spaceFactors = Codes.newSpaceFactors,
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
      csMap = mempty,
      catCodes = mempty,
      mathCodes = mempty,
      lowercaseCodes = mempty,
      uppercaseCodes = mempty,
      spaceFactors = mempty,
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
scopeResolvedTokenLens p = #csMap % at' p

-- | The cat-code for a char-code.
scopeCategoryLens :: Codes.CharCode -> Lens' Scope (Maybe Codes.CatCode)
scopeCategoryLens p = #catCodes % at' p

-- | The math-code for a char-code.
scopeMathCodeLens :: Codes.CharCode -> Lens' Scope (Maybe Codes.MathCode)
scopeMathCodeLens p = #mathCodes % at' p

-- | The change-case-code for a char-code, for a particular target-case.
scopeCaseChangeCodeLens :: ASCII.Case -> Codes.CharCode -> Lens' Scope (Maybe Codes.CaseChangeCode)
scopeCaseChangeCodeLens letterCase p = case letterCase of
  ASCII.LowerCase -> #lowercaseCodes % at' p
  ASCII.UpperCase -> #uppercaseCodes % at' p

-- | The space-factor-code for a char-code.
scopeSpaceFactorLens :: Codes.CharCode -> Lens' Scope (Maybe Codes.SpaceFactorCode)
scopeSpaceFactorLens p = #spaceFactors % at' p

-- | The delimiter-code for a char-code.
scopeDelimiterCodeLens :: Codes.CharCode -> Lens' Scope (Maybe Codes.DelimiterCode)
scopeDelimiterCodeLens p = #delimiterCodes % at' p

-- The value for an integer-parameter.
scopeIntParamLens :: PT.IntParameter -> Lens' Scope (Maybe H.Q.HexInt)
scopeIntParamLens p = #intParameters % at' p

-- The value for a length-parameter.
scopeLengthParamLens :: PT.LengthParameter -> Lens' Scope (Maybe H.Q.Length)
scopeLengthParamLens p = #lengthParameters % at' p

-- The value for a glue-parameter.
scopeGlueParamLens :: PT.GlueParameter -> Lens' Scope (Maybe H.Q.Glue)
scopeGlueParamLens p = #glueParameters % at' p
