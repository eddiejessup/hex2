module Hex.MonadHexState.Impls.HexState.Scope where

import Hex.Codes qualified as H.Codes
import Hex.MonadHexState.Impls.HexState.Parameters qualified as H.Inter.St.Param
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Initial qualified as H.Sym
import Hex.Symbol.Resolve qualified as H.Sym
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hex.Symbol.Token.Resolved qualified as H.Sym.Tok
import Hex.Symbol.Types qualified as H.Sym
import Hex.Evaluate.Syntax.Quantity qualified as H.Eval.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Syntax.Command qualified as H.Syn
import Hexlude
import qualified Hex.Syntax.Quantity as H.Syn

data Scope = Scope
  { -- Fonts.
    currentFontNr :: Maybe H.Sym.Tok.FontNumber,
    -- familyMemberFonts :: Map (FontRange, HexInt) HexInt,
    -- Control sequences.
    csMap :: H.Sym.CSMap,
    -- Char-code attribute maps.
    catCodes :: Map H.Codes.CharCode H.Codes.CatCode,
    mathCodes :: Map H.Codes.CharCode H.Codes.MathCode,
    lowercaseCodes :: Map H.Codes.CharCode H.Codes.CaseChangeCode,
    uppercaseCodes :: Map H.Codes.CharCode H.Codes.CaseChangeCode,
    spaceFactors :: Map H.Codes.CharCode H.Codes.SpaceFactorCode,
    delimiterCodes :: Map H.Codes.CharCode H.Codes.DelimiterCode,
    -- Parameters.
    intParameters :: Map H.Sym.Tok.IntParameter H.Q.HexInt,
    lengthParameters :: Map H.Sym.Tok.LengthParameter H.Q.Length,
    glueParameters :: Map H.Sym.Tok.GlueParameter H.Q.Glue,
    --   mathGlueParameters :: Map MathGlueParameter (BL.Glue MathLength),
    --   tokenListParameters :: Map TokenListParameter BalancedText
    -- Registers.
    intRegisters :: Map H.Eval.Syn.RegisterIndex H.Q.HexInt,
    lengthRegisters :: Map H.Eval.Syn.RegisterIndex H.Q.Length,
    glueRegisters :: Map H.Eval.Syn.RegisterIndex H.Q.Glue
    -- mathGlueRegisters :: RegisterMap (BL.Glue MathLength),
    --   tokenListRegisters :: RegisterMap BalancedText,
    --   boxRegisters :: RegisterMap (B.Box B.BoxContents),
  }
  deriving stock (Show, Generic)

newGlobalScope :: Scope
newGlobalScope =
  Scope
    { currentFontNr = Nothing,
      -- familyMemberFonts = mempty
      csMap = H.Sym.initialCSMap,
      catCodes = H.Codes.newCatCodes,
      mathCodes = H.Codes.newMathCodes,
      lowercaseCodes = H.Codes.newLowercaseCodes,
      uppercaseCodes = H.Codes.newUppercaseCodes,
      spaceFactors = H.Codes.newSpaceFactors,
      delimiterCodes = H.Codes.newDelimiterCodes,
      intParameters = H.Inter.St.Param.newIntParameters,
      lengthParameters = H.Inter.St.Param.newLengthParameters,
      glueParameters = H.Inter.St.Param.newGlueParameters,
      -- , mathGlueParameters = newMathGlueParameters
      -- , tokenListParameters = newTokenListParameters
      intRegisters = mempty,
      lengthRegisters = mempty,
      glueRegisters = mempty
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
      glueParameters = mempty,
      -- , mathGlueParameters = mempty
      -- , tokenListParameters = mempty
      intRegisters = mempty,
      lengthRegisters = mempty,
      glueRegisters = mempty
      -- , mathGlueRegister = mempty
      -- , tokenListRegister = mempty
      -- , boxRegister = mempty
    }

data HexGroup
  = ScopeGroup GroupScope
  | NonScopeGroup
  deriving stock (Generic)

data GroupScope = GroupScope {scgScope :: Scope, scgType :: GroupScopeType}
  deriving stock (Generic)

groupScopeATraversal :: AffineTraversal' HexGroup Scope
groupScopeATraversal = _Typed @GroupScope % typed @Scope

groupListScopeTraversal :: Traversal' [HexGroup] Scope
groupListScopeTraversal = traversed % groupScopeATraversal

data GroupScopeType
  = LocalStructureGroupScope H.Syn.CommandTrigger
  | ExplicitBoxGroupScope

scopeSymbolLens :: H.Sym.ControlSymbol -> Lens' Scope (Maybe H.Sym.Tok.ResolvedToken)
scopeSymbolLens p = #csMap % at' p

scopeCategoryLens :: H.Codes.CharCode -> Lens' Scope (Maybe H.Codes.CatCode)
scopeCategoryLens p = #catCodes % at' p

scopeIntVarLens :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.IntQuantity -> Lens' Scope (Maybe H.Q.HexInt)
scopeIntVarLens = \case
  H.Syn.ParamVar p -> #intParameters % at' p
  H.Syn.RegisterVar i -> #intRegisters % at' i

scopeLengthVarLens :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.LengthQuantity -> Lens' Scope (Maybe H.Q.Length)
scopeLengthVarLens = \case
  H.Syn.ParamVar p -> #lengthParameters % at' p
  H.Syn.RegisterVar i -> #lengthRegisters % at' i

scopeGlueVarLens :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.GlueQuantity -> Lens' Scope (Maybe H.Q.Glue)
scopeGlueVarLens = \case
  H.Syn.ParamVar p -> #glueParameters % at' p
  H.Syn.RegisterVar i -> #glueRegisters % at' i
