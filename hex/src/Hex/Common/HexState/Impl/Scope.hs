module Hex.Common.HexState.Impl.Scope where

import Hex.Common.Codes qualified as H.Codes
import Hex.Common.HexState.Impl.Parameters qualified as H.Inter.St.Param
import Hex.Stage.Parse.Interface.AST.Command qualified as H.Par.AST
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
    catCodes :: Map H.Codes.CharCode H.Codes.CatCode,
    mathCodes :: Map H.Codes.CharCode H.Codes.MathCode,
    lowercaseCodes :: Map H.Codes.CharCode H.Codes.CaseChangeCode,
    uppercaseCodes :: Map H.Codes.CharCode H.Codes.CaseChangeCode,
    spaceFactors :: Map H.Codes.CharCode H.Codes.SpaceFactorCode,
    delimiterCodes :: Map H.Codes.CharCode H.Codes.DelimiterCode,
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
      catCodes = H.Codes.newCatCodes,
      mathCodes = H.Codes.newMathCodes,
      lowercaseCodes = H.Codes.newLowercaseCodes,
      uppercaseCodes = H.Codes.newUppercaseCodes,
      spaceFactors = H.Codes.newSpaceFactors,
      delimiterCodes = H.Codes.newDelimiterCodes,
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

data HexGroup
  = ScopeGroup GroupScope
  | NonScopeGroup
  deriving stock (Show, Generic)

data GroupScope = GroupScope {scgScope :: Scope, scgType :: GroupScopeType}
  deriving stock (Show, Generic)

groupScopeATraversal :: AffineTraversal' HexGroup Scope
groupScopeATraversal = _Typed @GroupScope % typed @Scope

groupListScopeTraversal :: Traversal' [HexGroup] Scope
groupListScopeTraversal = traversed % groupScopeATraversal

data GroupScopeType
  = LocalStructureGroupScope H.Par.AST.CommandTrigger
  | ExplicitBoxGroupScope
  deriving stock (Show)

scopeResolvedTokenLens :: ControlSymbol -> Lens' Scope (Maybe ResolvedToken)
scopeResolvedTokenLens p = #csMap % at' p

scopeCategoryLens :: H.Codes.CharCode -> Lens' Scope (Maybe H.Codes.CatCode)
scopeCategoryLens p = #catCodes % at' p

scopeIntParamLens :: PT.IntParameter -> Lens' Scope (Maybe H.Q.HexInt)
scopeIntParamLens p = #intParameters % at' p

scopeLengthParamLens :: PT.LengthParameter -> Lens' Scope (Maybe H.Q.Length)
scopeLengthParamLens p = #lengthParameters % at' p

scopeGlueParamLens :: PT.GlueParameter -> Lens' Scope (Maybe H.Q.Glue)
scopeGlueParamLens p = #glueParameters % at' p
