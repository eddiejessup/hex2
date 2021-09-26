module Hex.MonadHexState.Interface where

import Hex.Codes qualified as H.Codes
import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Lex.Types qualified as H.Lex
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hex.Symbol.Token.Resolved qualified as H.Sym.Tok.Res
import Hex.Symbol.Types qualified as H.Sym
import Hex.Syntax.Font qualified as H.Syn
import Hex.Syntax.Quantity qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hexlude

class Monad m => MonadHexState m where
  getIntVariable :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.IntQuantity -> m H.Q.HexInt

  setIntVariable :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.IntQuantity -> H.Sym.Tok.ScopeFlag -> H.Q.HexInt -> m ()

  getLengthVariable :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.LengthQuantity -> m H.Q.Length

  setLengthVariable :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.LengthQuantity -> H.Sym.Tok.ScopeFlag -> H.Q.Length -> m ()

  getGlueVariable :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.GlueQuantity -> m H.Q.Glue

  setGlueVariable :: H.Syn.QuantVariable 'H.Syn.Evaluated 'H.Sym.Tok.GlueQuantity -> H.Sym.Tok.ScopeFlag -> H.Q.Glue -> m ()

  getSpecialIntParameter :: H.Sym.Tok.SpecialIntParameter -> m H.Q.HexInt

  setSpecialIntParameter :: H.Sym.Tok.SpecialIntParameter -> H.Q.HexInt -> m ()

  getSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> m H.Q.Length

  setSpecialLengthParameter :: H.Sym.Tok.SpecialLengthParameter -> H.Q.Length -> m ()

  getCategory :: H.Codes.CharCode -> m H.Codes.CatCode

  getSymbol :: H.Sym.ControlSymbol -> m (Maybe H.Sym.Tok.Res.ResolvedToken)

  setSymbol :: H.Sym.ControlSymbol -> H.Sym.Tok.ScopeFlag -> H.Sym.Tok.Res.ResolvedToken -> m ()

  loadFont :: H.Syn.HexFilePath -> H.Syn.FontSpecification 'H.Syn.Evaluated -> m H.Inter.B.Box.FontDefinition

  selectFont :: H.Sym.Tok.FontNumber -> H.Sym.Tok.ScopeFlag -> m ()

  currentFontCharacter :: H.Codes.CharCode -> m (Maybe (H.Q.Length, H.Q.Length, H.Q.Length, H.Q.Length))

  currentFontSpaceGlue :: m (Maybe H.Q.Glue)

  setAfterAssignmentToken :: Maybe H.Lex.LexToken -> m ()

-- Lifting.
instance {-# OVERLAPPABLE #-} MonadHexState m => MonadHexState (ExceptT e m) where
  getIntVariable = lift . getIntVariable

  getLengthVariable = lift . getLengthVariable

  getGlueVariable = lift . getGlueVariable

  getSpecialLengthParameter = lift . getSpecialLengthParameter

  setSpecialLengthParameter p v = lift $ setSpecialLengthParameter p v

  getCategory p = lift $ getCategory p

  getSymbol a = lift $ getSymbol a

  loadFont p spec = lift $ loadFont p spec

  selectFont a b = lift $ selectFont a b

  currentFontCharacter = lift . currentFontCharacter

  currentFontSpaceGlue = lift currentFontSpaceGlue

instance {-# OVERLAPPABLE #-} MonadHexState m => MonadHexState (StateT H.Inter.B.List.HList m)

instance {-# OVERLAPPABLE #-} MonadHexState m => MonadHexState (StateT H.Inter.B.List.VList m)
