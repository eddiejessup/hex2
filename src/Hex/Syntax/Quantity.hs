{-# LANGUAGE UndecidableInstances #-}
module Hex.Syntax.Quantity where

import Hexlude
import qualified Hex.Symbol.Token.Primitive as H.Sym.Tok
import qualified Hex.Quantity as H.Q
import Hex.Syntax.Common

type family HexPassInt (p :: Pass)
  -- HexPassInt 'Evaluated = H.Q.HexInt

type family HexPassLength (p :: Pass)
  -- HexPassLength 'Evaluated = H.Q.Length

type family HexPassMathLength (p :: Pass)
  -- HexPassMathLength 'Evaluated = H.Q.MathLength

type family HexPassGlue (p :: Pass)
  -- HexPassGlue 'Evaluated = H.Q.Glue

type family HexPassMathGlue (p :: Pass)

type family HexPassRegisterIndex (p :: Pass)

data CodeTableRef p = CodeTableRef H.Sym.Tok.CodeType (HexPassInt p)
  deriving stock (Generic)

type family QuantParam (a :: H.Sym.Tok.QuantityType) where
  QuantParam 'H.Sym.Tok.IntQuantity = H.Sym.Tok.IntParameter
  QuantParam 'H.Sym.Tok.LengthQuantity = H.Sym.Tok.LengthParameter
  QuantParam 'H.Sym.Tok.GlueQuantity = H.Sym.Tok.GlueParameter
  QuantParam 'H.Sym.Tok.MathGlueQuantity = H.Sym.Tok.MathGlueParameter
  QuantParam 'H.Sym.Tok.TokenListQuantity = H.Sym.Tok.TokenListParameter

data QuantVariable (p :: Pass) (a :: H.Sym.Tok.QuantityType) = ParamVar (QuantParam a) | RegisterVar (HexPassRegisterIndex p)
  deriving stock (Generic)

data FontRef p
  = FontTokenRef H.Sym.Tok.FontNumber
  | CurrentFontRef
  | FamilyMemberFontRef (FamilyMember p)
  deriving stock (Generic)

data FamilyMember p = FamilyMember H.Sym.Tok.FontRange (HexPassInt p)
  deriving stock (Generic)

deriving stock instance Eq (HexPassInt p) => Eq (FamilyMember p)

data FontCharRef p = FontCharRef H.Sym.Tok.FontChar (FontRef p)
  deriving stock (Generic)

data BoxDimensionRef p = BoxDimensionRef (HexPassInt p) H.Q.BoxDim
  deriving stock (Generic)

data FontDimensionRef p = FontDimensionRef (HexPassInt p) (FontRef p)
  deriving stock (Generic)

-- Internal quantities.

data InternalQuantity p
  = InternalIntQuantity (InternalInt p)
  | InternalLengthQuantity (InternalLength p)
  | InternalGlueQuantity (InternalGlue p)
  | InternalMathGlueQuantity (InternalMathGlue p)
  | FontQuantity (FontRef p)
  | TokenListVariableQuantity (QuantVariable p 'H.Sym.Tok.TokenListQuantity)
  deriving stock (Generic)

data InternalInt p
  = InternalIntVariable (QuantVariable p 'H.Sym.Tok.IntQuantity)
  | InternalSpecialIntParameter H.Sym.Tok.SpecialIntParameter
  | InternalCodeTableRef (CodeTableRef p)
  | InternalCharToken H.Q.HexInt
  | InternalMathCharToken H.Q.HexInt
  | InternalFontCharRef (FontCharRef p)
  | LastPenalty
  | ParShape
  | InputLineNr
  | Badness
  deriving stock (Generic)

data InternalLength p
  = InternalLengthVariable (QuantVariable p 'H.Sym.Tok.LengthQuantity)
  | InternalSpecialLengthParameter H.Sym.Tok.SpecialLengthParameter
  | InternalFontDimensionRef (FontDimensionRef p)
  | InternalBoxDimensionRef (BoxDimensionRef p)
  | LastKern
  deriving stock (Generic)

data InternalGlue p = InternalGlueVariable (QuantVariable p 'H.Sym.Tok.GlueQuantity) | LastGlue
  deriving stock (Generic)

data InternalMathGlue p
  = InternalMathGlueVariable (QuantVariable p 'H.Sym.Tok.MathGlueQuantity)
  | LastMathGlue
  deriving stock (Generic)
