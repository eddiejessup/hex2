module Hex.Common.HexState.Impl.Scoped.Scope where

import Data.Map.Strict qualified as Map
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Impl.SymbolMap (initialSymbolMap)
import Hex.Common.HexState.Interface.Resolve (SymbolMap)
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parameters qualified as Param
import Hex.Common.Quantity qualified as Q
import Hexlude

newtype RegisterLocation = RegisterLocation {unRegisterLocation :: Q.HexInt}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

fmtRegisterLocation :: Fmt RegisterLocation
fmtRegisterLocation = F.squared (F.accessed (.unRegisterLocation) Q.fmtHexInt)

type CharCodeMap v = Map Code.CharCode v

type RegisterMap v = Map RegisterLocation v

data Scope = Scope
  { -- Fonts.
    currentFontNr :: Maybe PT.FontNumber,
    -- familyMemberFonts :: Map (FontRange, HexInt) HexInt,
    -- Control sequences.
    symbolMap :: SymbolMap,
    -- Char-code attribute maps.
    catCodes :: CharCodeMap Code.CatCode,
    mathCodes :: CharCodeMap Code.MathCode,
    lowerCaseCodes :: CharCodeMap Code.LowerCaseCode,
    upperCaseCodes :: CharCodeMap Code.UpperCaseCode,
    spaceFactorCodes :: CharCodeMap Code.SpaceFactorCode,
    delimiterCodes :: CharCodeMap Code.DelimiterCode,
    -- Parameters.
    intParameters :: Map PT.IntParameter Q.HexInt,
    lengthParameters :: Map PT.LengthParameter Q.Length,
    glueParameters :: Map PT.GlueParameter Q.Glue,
    mathGlueParameters :: Map PT.MathGlueParameter Q.MathGlue,
    --   tokenListParameters :: Map TokenListParameter BalancedText
    -- Registers.
    intRegister :: RegisterMap Q.HexInt,
    lengthRegister :: RegisterMap Q.Length,
    glueRegister :: RegisterMap Q.Glue,
    mathGlueRegister :: RegisterMap Q.MathGlue
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
      catCodes = Code.newCatCodes,
      mathCodes = Code.newMathCodes,
      lowerCaseCodes = Code.newLowercaseCodes,
      upperCaseCodes = Code.newUppercaseCodes,
      spaceFactorCodes = Code.newspaceFactorCodes,
      delimiterCodes = Code.newDelimiterCodes,
      intParameters = Param.newIntParameters,
      lengthParameters = Param.newLengthParameters,
      glueParameters = Param.newGlueParameters,
      mathGlueParameters = Param.newMathGlueParameters,
      -- , tokenListParameters = newTokenListParameters
      intRegister = mempty,
      lengthRegister = mempty,
      glueRegister = mempty,
      mathGlueRegister = mempty
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
      mathGlueParameters = mempty,
      -- , tokenListParameters = mempty
      intRegister = mempty,
      lengthRegister = mempty,
      glueRegister = mempty,
      mathGlueRegister = mempty
      -- , tokenListRegister = mempty
      -- , boxRegister = mempty
    }

fmtScope :: Fmt Scope
fmtScope =
  ("Current font number: " |%| F.accessed (.currentFontNr) (F.maybed "None" PT.fmtFontNumber) |%| "\n")
    <> (fmtMapWithHeading "Symbols" (.symbolMap) Res.fmtControlSymbol Res.fmtResolvedToken)
    <> (fmtMapWithHeading "Category codes" (.catCodes) Code.fmtCharCode Code.fmtCatCode)
    <> (fmtMapWithHeading "Math codes" (.mathCodes) Code.fmtCharCode Code.fmtMathCode)
    <> (fmtMapWithHeading "Lowercase codes" (.lowerCaseCodes) Code.fmtCharCode Code.fmtLowerCaseCode)
    <> (fmtMapWithHeading "Uppercase codes" (.upperCaseCodes) Code.fmtCharCode Code.fmtUpperCaseCode)
    <> (fmtMapWithHeading "Space-factor codes" (.spaceFactorCodes) Code.fmtCharCode Code.fmtSpaceFactorCode)
    <> (fmtMapWithHeading "Delimiter codes" (.delimiterCodes) Code.fmtCharCode Code.fmtDelimiterCode)
    <> (fmtMapWithHeading "Int parameters" (.intParameters) PT.fmtIntParameter Q.fmtHexInt)
    <> (fmtMapWithHeading "Length parameters" (.lengthParameters) PT.fmtLengthParameter Q.fmtLengthWithUnit)
    <> (fmtMapWithHeading "Glue parameters" (.glueParameters) PT.fmtGlueParameter Q.fmtGlue)
    <> (fmtMapWithHeading "Int registers" (.intRegister) fmtRegisterLocation Q.fmtHexInt)
    <> (fmtMapWithHeading "Length registers" (.lengthRegister) fmtRegisterLocation Q.fmtLengthWithUnit)
    <> (fmtMapWithHeading "Glue registers" (.glueRegister) fmtRegisterLocation Q.fmtGlue)

-- If we have two scopes, one nested inside another, we want to consider the
-- effective scope seen in the inner scope.
-- Map.union is left-biased, i.e. if keys exist in left and right, the union
-- result contains the left value. This is what we want, as the inner scope
-- should be the value we observe.
instance Semigroup Scope where
  innerScope <> outerScope =
    Scope
      { currentFontNr = innerScope.currentFontNr <|> outerScope.currentFontNr,
        symbolMap = innerScope.symbolMap `Map.union` outerScope.symbolMap,
        catCodes = innerScope.catCodes `Map.union` outerScope.catCodes,
        mathCodes = innerScope.mathCodes `Map.union` outerScope.mathCodes,
        lowerCaseCodes = innerScope.lowerCaseCodes `Map.union` outerScope.lowerCaseCodes,
        upperCaseCodes = innerScope.upperCaseCodes `Map.union` outerScope.upperCaseCodes,
        spaceFactorCodes = innerScope.spaceFactorCodes `Map.union` outerScope.spaceFactorCodes,
        delimiterCodes = innerScope.delimiterCodes `Map.union` outerScope.delimiterCodes,
        intParameters = innerScope.intParameters `Map.union` outerScope.intParameters,
        lengthParameters = innerScope.lengthParameters `Map.union` outerScope.lengthParameters,
        glueParameters = innerScope.glueParameters `Map.union` outerScope.glueParameters,
        mathGlueParameters = innerScope.mathGlueParameters `Map.union` outerScope.mathGlueParameters,
        intRegister = innerScope.intRegister `Map.union` outerScope.intRegister,
        lengthRegister = innerScope.lengthRegister `Map.union` outerScope.lengthRegister,
        glueRegister = innerScope.glueRegister `Map.union` outerScope.glueRegister,
        mathGlueRegister = innerScope.mathGlueRegister `Map.union` outerScope.mathGlueRegister
      }

-- If we introduce a new local scope, our effective seen-scope should be
-- unchanged.
instance Monoid Scope where
  mempty = newLocalScope
