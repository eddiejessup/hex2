{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.Syntax.Command where

import Hexlude
import Hex.Parse.Syntax.Quantity
import Hex.Lex.Types qualified as H.Lex
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hex.Symbol.Token.SyntaxCommandHead qualified as H.Sym.Tok
import Hex.Syntax.Font qualified as H.Syn
import Hex.Syntax.Command as H.Syn
import Hex.Syntax.Common as H.Syn
import Hex.Syntax.Quantity as H.Syn
import qualified Hex.Quantity as H.Q

type instance H.Syn.HexPassControlSequenceTarget 'H.Syn.Parsed = ControlSequenceTarget
type instance H.Syn.HexPassCharList 'H.Syn.Parsed = H.Sym.Tok.ExpandedBalancedText
type instance H.Syn.HexPassRule 'H.Syn.Parsed = Rule

data ControlSequenceTarget
  = MacroTarget H.Sym.Tok.MacroDefinition
  | LetTarget H.Lex.LexToken
  | FutureLetTarget H.Lex.LexToken H.Lex.LexToken
  | ShortDefineTarget H.Sym.Tok.CharryQuantityType HexInt
  | ReadTarget HexInt
  | FontTarget (H.Syn.FontFileSpec 'H.Syn.Parsed)
  deriving stock (Generic)

newtype Rule = Rule (Seq (H.Q.BoxDim, H.Syn.HexPassLength 'H.Syn.Parsed))
  deriving stock (Generic)
