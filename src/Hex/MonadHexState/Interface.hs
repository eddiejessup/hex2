module Hex.MonadHexState.Interface where

import Hex.Codes qualified as H.Codes
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Lex.Types qualified as H.Lex
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hex.Symbol.Token.Resolved qualified as H.Sym.Tok.Res
import Hex.Symbol.Types qualified as H.Sym
import Hexlude

class Monad m => MonadHexState m where
  getIntParameter :: H.Sym.Tok.IntParameter -> m H.Q.HexInt

  getLengthParameter :: H.Sym.Tok.LengthParameter -> m H.Q.Length

  getGlueParameter :: H.Sym.Tok.GlueParameter -> m H.Q.Glue

  getSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> m H.Q.Length

  setSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> H.Q.Length -> m ()

  getCategory :: H.Codes.CharCode -> m H.Codes.CatCode

  resolveSymbol :: H.Sym.ControlSymbol -> m (Maybe H.Sym.Tok.Res.ResolvedToken)

  loadFont :: H.Inter.B.Box.HexFilePath -> H.Inter.B.Box.FontSpecification -> m H.Inter.B.Box.FontDefinition

  selectFont :: H.Sym.Tok.FontNumber -> H.Sym.Tok.ScopeFlag -> m ()

  currentFontCharacter :: H.Codes.CharCode -> m (Maybe (H.Q.Length, H.Q.Length, H.Q.Length, H.Q.Length))

  currentFontSpaceGlue :: m (Maybe H.Q.Glue)

  setAfterAssignmentToken :: Maybe H.Lex.LexToken -> m ()

  setControlSequence :: H.Sym.ControlSymbol -> H.Sym.Tok.Res.ResolvedToken -> H.Sym.Tok.ScopeFlag -> m ()

-- Lifting.
instance {-# OVERLAPPABLE #-} MonadHexState m => MonadHexState (ExceptT e m) where
  getIntParameter = lift . getIntParameter

  getLengthParameter = lift . getLengthParameter

  getGlueParameter = lift . getGlueParameter

  getSpecialLengthParameter = lift . getSpecialLengthParameter

  setSpecialLengthParameter p v = lift $ setSpecialLengthParameter p v

  getCategory p = lift $ getCategory p

  resolveSymbol a = lift $ resolveSymbol a

  loadFont p spec = lift $ loadFont p spec

  selectFont a b = lift $ selectFont a b

  currentFontCharacter = lift . currentFontCharacter

  currentFontSpaceGlue = lift currentFontSpaceGlue

instance {-# OVERLAPPABLE #-} MonadHexState m => MonadHexState (StateT H.Inter.B.List.HList m)

instance {-# OVERLAPPABLE #-} MonadHexState m => MonadHexState (StateT H.Inter.B.List.VList m)
