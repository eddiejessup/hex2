{-# LANGUAGE UndecidableInstances #-}

module Hex.Syntax.Command where

import Hex.Codes qualified as H.Code
import Hex.Lex.Types qualified as H.Lex
import Hex.Syntax.Quantity
import Hex.Syntax.Common
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive qualified as H.Sym.Tok
import Hex.Symbol.Token.SyntaxCommandHead qualified as H.Sym.Tok.Syn
import Hex.Symbol.Types qualified as H.Sym
import Hexlude

type family HexPassControlSequenceTarget (p :: Pass)
type family HexPassCharList (p :: Pass)
type family HexPassRule (p :: Pass)

  -- HexPassControlSequenceTarget 'Parsed = H.Par.Syn.ControlSequenceTarget
  -- HexPassControlSequenceTarget 'Evaluated = H.Eval.Syn.ControlSequenceTarget

data Command (p :: Pass)
  = ShowToken H.Lex.LexToken
  | ShowBox (HexPassInt p)
  | ShowLists
  | ShowTheInternalQuantity (InternalQuantity p)
  | ShipOut (Box p)
  | AddMark H.Sym.Tok.Syn.ExpandedBalancedText
  | -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- -- then can 'migrate' out.
    -- \| AddInsertion (HexPassInt p) VModeMaterial
    -- \| AddAdjustment VModeMaterial
    AddSpace
  | StartParagraph H.Sym.Tok.IndentFlag
  | EndParagraph
  | -- \| AddAlignedMaterial DesiredLength AlignmentMaterial
    HModeCommand (HModeCommand p)
  | VModeCommand (VModeCommand p)
  | ModeIndependentCommand (ModeIndependentCommand p)
  deriving stock (Generic)

data ModeIndependentCommand p
  = Assign (Assignment p)
  | Relax
  | IgnoreSpaces
  | AddPenalty (HexPassInt p)
  | AddKern (HexPassLength p)
  | AddMathKern (HexPassMathLength p)
  | RemoveItem H.Sym.Tok.RemovableItem
  | SetAfterAssignmentToken H.Lex.LexToken
  | AddToAfterGroupTokens H.Lex.LexToken
  | WriteMessage (MessageWriteCommand p)
  | ModifyFileStream (FileStreamModificationCommand p)
  | WriteToStream (StreamWriteCommand p)
  | DoSpecial (HexPassCharList p)
  | AddBox (BoxPlacement p) (Box p)
  | ChangeScope H.Q.Sign CommandTrigger
  deriving stock (Generic)

data VModeCommand p
  = End
  | Dump
  | EnterHMode
  | AddVGlue (HexPassGlue p)
  | AddVLeaders (LeadersSpec p)
  | AddVRule (HexPassRule p)
  | AddUnwrappedFetchedVBox (FetchedBoxRef p) -- \unv{box,copy}
  deriving stock (Generic)

data HModeCommand p
  = AddControlSpace
  | AddCharacter (CharCodeRef p)
  | AddAccentedCharacter (HexPassInt p) [Assignment p] (Maybe (CharCodeRef p))
  | AddItalicCorrection
  | AddDiscretionaryText DiscretionaryText
  | AddDiscretionaryHyphen
  | EnterMathMode
  | AddHGlue (HexPassGlue p)
  | AddHLeaders (LeadersSpec p)
  | AddHRule (HexPassRule p)
  | AddUnwrappedFetchedHBox (FetchedBoxRef p) -- \unh{box,copy}
  deriving stock (Generic)

data StreamWriteCommand p = StreamWriteCommand (HexPassInt p) (WriteText p)
  deriving stock (Generic)

data MessageWriteCommand p = MessageWriteCommand H.Sym.Tok.StandardOutputStream (HexPassCharList p)
  deriving stock (Generic)

data FileStreamModificationCommand p = FileStreamModificationCommand FileStreamType FileStreamAction (HexPassInt p)
  deriving stock (Generic)

data Assignment p = Assignment {body :: AssignmentBody p, scope :: H.Sym.Tok.ScopeFlag}
  deriving stock (Generic)

data AssignmentBody p
  = DefineControlSequence H.Sym.ControlSymbol (HexPassControlSequenceTarget p)
  | SetVariable (VariableAssignment p)
  | ModifyVariable (VariableModification p)
  | AssignCode (CodeAssignment p)
  | SelectFont H.Sym.Tok.FontNumber
  | SetFamilyMember (FamilyMember p) (FontRef p)
  | SetParShape (HexPassInt p) [HexPassLength p]
  | SetBoxRegister (HexPassInt p) (Box p)
  | -- Global assignments.
    SetFontDimension (FontDimensionRef p) (HexPassLength p)
  | SetFontChar (FontCharRef p) (HexPassInt p)
  | SetHyphenation H.Sym.Tok.Syn.InhibitedBalancedText
  | SetHyphenationPatterns H.Sym.Tok.Syn.InhibitedBalancedText
  | SetBoxDimension (BoxDimensionRef p) (HexPassLength p)
  | SetInteractionMode H.Sym.Tok.InteractionMode
  deriving stock (Generic)

data TokenListAssignmentTarget p
  = TokenListAssignmentVar (QuantVariable p 'H.Sym.Tok.TokenListQuantity)
  | TokenListAssignmentText H.Sym.Tok.Syn.InhibitedBalancedText
  deriving stock (Generic)

data QuantVariableAssignment (p :: Pass) (q :: H.Sym.Tok.QuantityType) = QuantVariableAssignment (QuantVariable p q) (QuantVariableTarget p q)
  deriving stock (Generic)

type family QuantVariableTarget (p :: Pass) (a :: H.Sym.Tok.QuantityType) where
  QuantVariableTarget p 'H.Sym.Tok.IntQuantity = (HexPassInt p)
  QuantVariableTarget p 'H.Sym.Tok.LengthQuantity = (HexPassLength p)
  QuantVariableTarget p 'H.Sym.Tok.GlueQuantity = (HexPassGlue p)
  QuantVariableTarget p 'H.Sym.Tok.MathGlueQuantity = (HexPassMathGlue p)
  QuantVariableTarget p 'H.Sym.Tok.TokenListQuantity = (TokenListAssignmentTarget p)

data VariableAssignment p
  = IntVariableAssignment (QuantVariableAssignment p 'H.Sym.Tok.IntQuantity)
  | LengthVariableAssignment (QuantVariableAssignment p 'H.Sym.Tok.LengthQuantity)
  | GlueVariableAssignment (QuantVariableAssignment p 'H.Sym.Tok.GlueQuantity)
  | MathGlueVariableAssignment (QuantVariableAssignment p 'H.Sym.Tok.MathGlueQuantity)
  | TokenListVariableAssignment (QuantVariableAssignment p 'H.Sym.Tok.TokenListQuantity)
  | SpecialIntParameterAssignment H.Sym.Tok.SpecialIntParameter (HexPassInt p)
  | SpecialLengthParameterAssignment H.Sym.Tok.SpecialLengthParameter (HexPassLength p)
  deriving stock (Generic)

data VariableModification p
  = AdvanceIntVariable (QuantVariable p 'H.Sym.Tok.IntQuantity) (QuantVariableTarget p 'H.Sym.Tok.IntQuantity)
  | AdvanceLengthVariable (QuantVariable p 'H.Sym.Tok.LengthQuantity) (QuantVariableTarget p 'H.Sym.Tok.LengthQuantity)
  | AdvanceGlueVariable (QuantVariable p 'H.Sym.Tok.GlueQuantity) (QuantVariableTarget p 'H.Sym.Tok.GlueQuantity)
  | AdvanceMathGlueVariable (QuantVariable p 'H.Sym.Tok.MathGlueQuantity) (QuantVariableTarget p 'H.Sym.Tok.MathGlueQuantity)
  | ScaleVariable H.Q.VDirection (NumericVariable p) (HexPassInt p)
  deriving stock (Generic)

data NumericVariable p
  = IntNumericVariable (QuantVariable p 'H.Sym.Tok.IntQuantity)
  | LengthNumericVariable (QuantVariable p 'H.Sym.Tok.LengthQuantity)
  | GlueNumericVariable (QuantVariable p 'H.Sym.Tok.GlueQuantity)
  | MathGlueNumericVariable (QuantVariable p 'H.Sym.Tok.MathGlueQuantity)
  deriving stock (Generic)

data CodeAssignment p = CodeAssignment (CodeTableRef p) (HexPassInt p)
  deriving stock (Generic)

-- Box specification.
data Box p
  = FetchedRegisterBox H.Sym.Tok.BoxFetchMode (HexPassInt p)
  | LastBox
  | VSplitBox (HexPassInt p) (HexPassLength p)
  | ExplicitBox (BoxSpecification p) H.Sym.Tok.ExplicitBox
  deriving stock (Generic)

data BoxSpecification p = Natural | To (HexPassLength p) | Spread (HexPassLength p)
  deriving stock (Generic)

data BoxOrRule p = BoxOrRuleBox (Box p) | BoxOrRuleRule H.Q.Axis (HexPassRule p)
  deriving stock (Generic)

data DiscretionaryText = DiscretionaryText {preBreak, postBreak, noBreak :: H.Sym.Tok.Syn.ExpandedBalancedText}
  deriving stock (Generic)

data FetchedBoxRef p = FetchedBoxRef (HexPassInt p) H.Sym.Tok.BoxFetchMode
  deriving stock (Generic)

data LeadersSpec p = LeadersSpec H.Sym.Tok.LeadersType (BoxOrRule p) (HexPassGlue p)
  deriving stock (Generic)

data CommandTrigger = CharCommandTrigger | CSCommandTrigger
  deriving stock (Generic)

data WriteText p
  = ImmediateWriteText (HexPassCharList p)
  | DeferredWriteText H.Sym.Tok.Syn.InhibitedBalancedText
  deriving stock (Generic)

data WritePolicy = Immediate | Deferred
  deriving stock (Generic)

data FileStreamAction = Open HexFilePath | Close
  deriving stock (Generic)

data FileStreamType = FileInput | FileOutput WritePolicy
  deriving stock (Generic)

data BoxPlacement p = NaturalPlacement | ShiftedPlacement H.Q.Axis H.Q.Direction (HexPassLength p)
  deriving stock (Generic)

data CharCodeRef p
  = CharRef H.Code.CharCode
  | CharTokenRef H.Q.HexInt
  | CharCodeNrRef (HexPassInt p)
  deriving stock (Generic)
