module Hex.Common.HexState.Interface where

import Hex.Common.Codes qualified as H.Codes
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as H.Sym.Tok
import Hex.Common.Quantity qualified as H.Q
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hexlude

class Monad m => MonadHexState m where
  getIntParameter :: H.Sym.Tok.IntParameter -> m H.Q.HexInt

  getLengthParameter :: H.Sym.Tok.LengthParameter -> m H.Q.Length

  getGlueParameter :: H.Sym.Tok.GlueParameter -> m H.Q.Glue

  getSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> m H.Q.Length

  setSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> H.Q.Length -> m ()

  getCategory :: H.Codes.CharCode -> m H.Codes.CatCode

  resolveSymbol :: ControlSymbol -> m (Maybe ResolvedToken)

  loadFont :: H.Inter.B.Box.HexFilePath -> H.Inter.B.Box.FontSpecification -> m H.Inter.B.Box.FontDefinition

  selectFont :: H.Sym.Tok.FontNumber -> H.Sym.Tok.ScopeFlag -> m ()

  currentFontCharacter :: H.Codes.CharCode -> m (Maybe (H.Q.Length, H.Q.Length, H.Q.Length, H.Q.Length))

  currentFontSpaceGlue :: m (Maybe H.Q.Glue)

  setAfterAssignmentToken :: Maybe Lex.LexToken -> m ()

  setControlSequence :: ControlSymbol -> ResolvedToken -> H.Sym.Tok.ScopeFlag -> m ()

  -- Support stuff for parsing.
  setLastFetchedLexTok :: Lex.LexToken -> m ()

  getLastFetchedLexTok :: m (Maybe Lex.LexToken)

-- Lifting.
-- instance {-# OVERLAPPABLE #-} MonadHexState m => MonadHexState (ExceptT e m) where
--   getIntParameter = lift . getIntParameter

--   getLengthParameter = lift . getLengthParameter

--   getGlueParameter = lift . getGlueParameter

--   getSpecialLengthParameter = lift . getSpecialLengthParameter

--   setSpecialLengthParameter p v = lift $ setSpecialLengthParameter p v

--   getCategory p = lift $ getCategory p

--   resolveSymbol a = lift $ resolveSymbol a

--   loadFont p spec = lift $ loadFont p spec

--   selectFont a b = lift $ selectFont a b

--   currentFontCharacter = lift . currentFontCharacter

--   currentFontSpaceGlue = lift currentFontSpaceGlue

instance MonadHexState m => MonadHexState (StateT H.Inter.B.List.HList m) where
  getIntParameter = panic "Not implemented"
  getLengthParameter = panic "Not implemented"
  getGlueParameter = panic "Not implemented"
  getSpecialLengthParameter = panic "Not implemented"
  setSpecialLengthParameter = panic "Not implemented"
  getCategory = panic "Not implemented"
  resolveSymbol = panic "Not implemented"
  loadFont = panic "Not implemented"
  selectFont = panic "Not implemented"
  currentFontCharacter = panic "Not implemented"
  currentFontSpaceGlue = panic "Not implemented"
  setAfterAssignmentToken = panic "Not implemented"
  setControlSequence = panic "Not implemented"
  setLastFetchedLexTok = panic "Not implemented"
  getLastFetchedLexTok = panic "Not implemented"

instance MonadHexState m => MonadHexState (StateT H.Inter.B.List.VList m) where
  getIntParameter = panic "Not implemented"
  getLengthParameter = panic "Not implemented"
  getGlueParameter = panic "Not implemented"
  getSpecialLengthParameter = panic "Not implemented"
  setSpecialLengthParameter = panic "Not implemented"
  getCategory = panic "Not implemented"
  resolveSymbol = panic "Not implemented"
  loadFont = panic "Not implemented"
  selectFont = panic "Not implemented"
  currentFontCharacter = panic "Not implemented"
  currentFontSpaceGlue = panic "Not implemented"
  setAfterAssignmentToken = panic "Not implemented"
  setControlSequence = panic "Not implemented"
  setLastFetchedLexTok = panic "Not implemented"
  getLastFetchedLexTok = panic "Not implemented"

getParIndentBox :: MonadHexState m => m H.Inter.B.List.HListElem
getParIndentBox = do
  boxWidth <- getLengthParameter H.Sym.Tok.ParIndent
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
