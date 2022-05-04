module Hex.Common.HexState.Interface where

import Hex.Common.Codes qualified as Code
import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Impl.GroupScopes (MutableHexCode)
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

class Monad m => MonadHexState m where
  getIntParameter :: PT.IntParameter -> m Q.HexInt

  getLengthParameter :: PT.LengthParameter -> m Q.Length

  getGlueParameter :: PT.GlueParameter -> m Q.Glue

  getSpecialIntParameter :: PT.SpecialIntParameter -> m Q.HexInt

  setSpecialIntParameter :: PT.SpecialIntParameter -> Q.HexInt -> m ()

  getSpecialLengthParameter :: PT.SpecialLengthParameter -> m Q.Length

  setSpecialLengthParameter :: PT.SpecialLengthParameter -> Q.Length -> m ()

  getHexCode :: MutableHexCode c => Codes.CharCode -> m c

  setHexCode :: MutableHexCode c => Code.CharCode -> c -> PT.ScopeFlag -> m ()

  resolveSymbol :: ControlSymbol -> m (Maybe ResolvedToken)

  loadFont :: H.Inter.B.Box.HexFilePath -> H.Inter.B.Box.FontSpecification -> m H.Inter.B.Box.FontDefinition

  selectFont :: PT.FontNumber -> PT.ScopeFlag -> m ()

  currentFontCharacter :: Codes.CharCode -> m (Maybe (Q.Length, Q.Length, Q.Length, Q.Length))

  currentFontSpaceGlue :: m (Maybe Q.Glue)

  popAfterAssignmentToken :: m (Maybe Lex.LexToken)

  setAfterAssignmentToken :: Lex.LexToken -> m ()

  setSymbol :: ControlSymbol -> ResolvedToken -> PT.ScopeFlag -> m ()

  -- Support stuff for parsing.
  setLastFetchedLexTok :: Lex.LexToken -> m ()

  getLastFetchedLexTok :: m (Maybe Lex.LexToken)

instance MonadHexState m => MonadHexState (StateT a m) where
  getIntParameter x = lift $ getIntParameter x
  getLengthParameter x = lift $ getLengthParameter x
  getGlueParameter x = lift $ getGlueParameter x
  getSpecialIntParameter x = lift $ getSpecialIntParameter x
  setSpecialIntParameter x y = lift $ setSpecialIntParameter x y
  getSpecialLengthParameter x = lift $ getSpecialLengthParameter x
  setSpecialLengthParameter x y = lift $ setSpecialLengthParameter x y
  getHexCode x = lift $ getHexCode x
  setHexCode x y z = lift $ setHexCode x y z
  resolveSymbol x = lift $ resolveSymbol x
  loadFont x y = lift $ loadFont x y
  selectFont x y = lift $ selectFont x y
  currentFontCharacter x = lift $ currentFontCharacter x
  currentFontSpaceGlue = lift currentFontSpaceGlue
  popAfterAssignmentToken = lift popAfterAssignmentToken
  setAfterAssignmentToken x = lift $ setAfterAssignmentToken x
  setSymbol x y z = lift $ setSymbol x y z
  setLastFetchedLexTok x = lift $ setLastFetchedLexTok x
  getLastFetchedLexTok = lift getLastFetchedLexTok

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
