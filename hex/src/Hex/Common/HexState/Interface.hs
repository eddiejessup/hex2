module Hex.Common.HexState.Interface where

import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude
import qualified Hex.Common.Codes as Code
import qualified ASCII

class Monad m => MonadHexState m where
  getIntParameter :: PT.IntParameter -> m Q.HexInt

  getLengthParameter :: PT.LengthParameter -> m Q.Length

  getGlueParameter :: PT.GlueParameter -> m Q.Glue

  getSpecialLengthParameter :: PT.SpecialLengthParameter -> m Q.Length

  setSpecialLengthParameter :: PT.SpecialLengthParameter -> Q.Length -> m ()

  getCategory :: Codes.CharCode -> m Codes.CatCode

  resolveSymbol :: ControlSymbol -> m (Maybe ResolvedToken)

  loadFont :: H.Inter.B.Box.HexFilePath -> H.Inter.B.Box.FontSpecification -> m H.Inter.B.Box.FontDefinition

  selectFont :: PT.FontNumber -> PT.ScopeFlag -> m ()

  setCategory :: Code.CharCode -> Code.CatCode -> PT.ScopeFlag -> m ()

  setMathCode :: Code.CharCode -> Code.MathCode -> PT.ScopeFlag -> m ()

  setChangeCaseCode :: ASCII.Case -> Code.CharCode -> Code.CaseChangeCode -> PT.ScopeFlag -> m ()

  setSpaceFactor :: Code.CharCode -> Code.SpaceFactorCode -> PT.ScopeFlag -> m ()

  setDelimiterCode :: Code.CharCode -> Code.DelimiterCode -> PT.ScopeFlag -> m ()

  currentFontCharacter :: Codes.CharCode -> m (Maybe (Q.Length, Q.Length, Q.Length, Q.Length))

  currentFontSpaceGlue :: m (Maybe Q.Glue)

  setAfterAssignmentToken :: Maybe Lex.LexToken -> m ()

  setControlSequence :: ControlSymbol -> ResolvedToken -> PT.ScopeFlag -> m ()

  -- Support stuff for parsing.
  setLastFetchedLexTok :: Lex.LexToken -> m ()

  getLastFetchedLexTok :: m (Maybe Lex.LexToken)

instance MonadHexState m => MonadHexState (StateT H.Inter.B.List.HList m) where
  getIntParameter x = lift $ getIntParameter x
  getLengthParameter x = lift $ getLengthParameter x
  getGlueParameter x = lift $ getGlueParameter x
  getSpecialLengthParameter x = lift $ getSpecialLengthParameter x
  setSpecialLengthParameter x y = lift $ setSpecialLengthParameter x y
  getCategory x = lift $ getCategory x
  resolveSymbol x = lift $ resolveSymbol x
  loadFont x y = lift $ loadFont x y
  selectFont x y = lift $ selectFont x y
  currentFontCharacter x = lift $ currentFontCharacter x
  currentFontSpaceGlue = lift currentFontSpaceGlue
  setAfterAssignmentToken x = lift $ setAfterAssignmentToken x
  setControlSequence x y z = lift $ setControlSequence x y z
  setLastFetchedLexTok x = lift $ setLastFetchedLexTok x
  getLastFetchedLexTok = lift getLastFetchedLexTok
  setCategory x y z = lift $ setCategory x y z
  setMathCode x y z = lift $ setMathCode x y z
  setChangeCaseCode w x y z = lift $ setChangeCaseCode w x y z
  setSpaceFactor x y z = lift $ setSpaceFactor x y z
  setDelimiterCode x y z = lift $ setDelimiterCode x y z

instance MonadHexState m => MonadHexState (StateT H.Inter.B.List.VList m) where
  getIntParameter x = lift $ getIntParameter x
  getLengthParameter x = lift $ getLengthParameter x
  getGlueParameter x = lift $ getGlueParameter x
  getSpecialLengthParameter x = lift $ getSpecialLengthParameter x
  setSpecialLengthParameter x y = lift $ setSpecialLengthParameter x y
  getCategory x = lift $ getCategory x
  resolveSymbol x = lift $ resolveSymbol x
  loadFont x y = lift $ loadFont x y
  selectFont x y = lift $ selectFont x y
  currentFontCharacter x = lift $ currentFontCharacter x
  currentFontSpaceGlue = lift currentFontSpaceGlue
  setAfterAssignmentToken x = lift $ setAfterAssignmentToken x
  setControlSequence x y z = lift $ setControlSequence x y z
  setLastFetchedLexTok x = lift $ setLastFetchedLexTok x
  getLastFetchedLexTok = lift getLastFetchedLexTok
  setCategory x y z = lift $ setCategory x y z
  setMathCode x y z = lift $ setMathCode x y z
  setChangeCaseCode w x y z = lift $ setChangeCaseCode w x y z
  setSpaceFactor x y z = lift $ setSpaceFactor x y z
  setDelimiterCode x y z = lift $ setDelimiterCode x y z

getParIndentBox :: MonadHexState m => m H.Inter.B.List.HListElem
getParIndentBox = do
  boxWidth <- getLengthParameter PT.ParIndent
  pure $
    H.Inter.B.List.HVListElem $
      H.Inter.B.List.VListBaseElem $
        H.Inter.B.Box.ElemBox $
          H.Inter.B.Box.Box
            { H.Inter.B.Box.contents = H.Inter.B.Box.HBoxContents (H.Inter.B.Box.HBoxElemSeq Empty),
              H.Inter.B.Box.boxWidth,
              H.Inter.B.Box.boxHeight = mempty,
              H.Inter.B.Box.boxDepth = mempty
            }
