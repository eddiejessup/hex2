module Hex.HexState.Scope where

import Data.Generics.Product qualified as G.P
import Hex.Codes qualified as H.Codes
import Hex.HexState.Parameters qualified as H.Inter.St.Param
import Hex.Interpret.Evaluate.Evaluated qualified as H.Inter.Eval
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.AST qualified as H.Par.AST
import Hex.Symbol.Initial qualified as H.Sym
import Hex.Symbol.Resolve qualified as H.Sym
import Hex.Symbol.Tokens qualified as H.Sym.Tok
-- Import `Optics.At` for instance:
--   (Eq k, Hashable k) => At (HashMap k a)
-- So we can do `at` on a HashMap, for control sequence map
import Optics.At ()
import Optics.Core ((%))
import Optics.Core qualified as O
import Protolude hiding ((%))

data Scope = Scope
  { -- Fonts.
    --     currentFontNr :: Maybe HexInt
    --   familyMemberFonts :: Map (FontRange, HexInt) HexInt,
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
    intParameters :: Map H.Sym.Tok.IntParameter H.Inter.Eval.HexInt,
    lengthParameters :: Map H.Sym.Tok.LengthParameter H.Inter.Eval.Length,
    glueParameters :: Map H.Sym.Tok.GlueParameter (H.Inter.Eval.Glue H.Inter.Eval.Length)
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
  Scope --currentFontNr = Nothing
  -- , familyMemberFonts = mempty
    { csMap = H.Sym.initialCSMap,
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
  Scope --currentFontNr = Nothing
  -- , familyMemberFonts = mempty
    { csMap = mempty,
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

data Group
  = ScopeGroup Scope ScopeGroup
  | NonScopeGroup
  deriving stock (Show)

groupScope :: Group -> Maybe Scope
groupScope = \case
  ScopeGroup scope _ ->
    Just scope
  _ ->
    Nothing

groupScopes :: [Group] -> [Scope]
groupScopes = mapMaybe groupScope

data ScopeGroup
  = LocalStructureGroup H.Par.AST.CommandTrigger
  | ExplicitBoxGroup
  deriving stock (Show)

scopeResolvedTokenLens :: H.Lex.LexSymbol -> O.Lens' Scope (Maybe H.Sym.Tok.ResolvedToken)
scopeResolvedTokenLens p = G.P.field @"csMap" % O.at' p

scopeCategoryLens :: H.Codes.CharCode -> O.Lens' Scope (Maybe H.Codes.CatCode)
scopeCategoryLens p = G.P.field @"catCodes" % O.at' p

scopeIntParamLens :: H.Sym.Tok.IntParameter -> O.Lens' Scope (Maybe H.Inter.Eval.HexInt)
scopeIntParamLens p = G.P.field @"intParameters" % O.at' p

scopeLengthParamLens :: H.Sym.Tok.LengthParameter -> O.Lens' Scope (Maybe H.Inter.Eval.Length)
scopeLengthParamLens p = G.P.field @"lengthParameters" % O.at' p

scopeGlueParamLens :: H.Sym.Tok.GlueParameter -> O.Lens' Scope (Maybe (H.Inter.Eval.Glue H.Inter.Eval.Length))
scopeGlueParamLens p = G.P.field @"glueParameters" % O.at' p
