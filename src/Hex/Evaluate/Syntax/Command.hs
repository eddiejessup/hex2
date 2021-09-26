{-# LANGUAGE UndecidableInstances #-}

module Hex.Evaluate.Syntax.Command where

import Hex.Symbol.Token.Resolved qualified as H.Sym.Tok.Res
import Hexlude
import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Syntax.Font qualified as H.Syn
import qualified Hex.Interpret.Build.Box.Elem as H.Inter.B.Box

type instance H.Syn.HexPassControlSequenceTarget 'H.Syn.Evaluated = ControlSequenceTarget
type instance H.Syn.HexPassCharList 'H.Syn.Evaluated = ByteString
type instance H.Syn.HexPassRule 'H.Syn.Evaluated = H.Inter.B.Box.Rule

data ControlSequenceTarget
  = ResolvedTokenTarget H.Sym.Tok.Res.ResolvedToken
  | FontTarget (H.Syn.FontFileSpec 'H.Syn.Evaluated)
