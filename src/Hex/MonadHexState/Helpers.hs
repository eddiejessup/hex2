module Hex.MonadHexState.Helpers where

import Hex.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.MonadHexState.Interface
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hexlude
import qualified Hex.Syntax.Quantity as H.Syn

getParIndentBox :: MonadHexState m => m H.Inter.B.List.HListElem
getParIndentBox = do
  boxWidth <- getLengthVariable (H.Syn.ParamVar H.Sym.Tok.ParIndent)
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
