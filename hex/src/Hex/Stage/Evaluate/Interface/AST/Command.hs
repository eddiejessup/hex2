{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Evaluate.Interface.AST.Command where

import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Group
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Hyphen qualified as HSt.Hyph
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Register
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hex.Common.Quantity qualified as Q
import Hex.Common.TFM.Types qualified as TFM
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Build.AnyDirection.Breaking.Badness qualified as Bad
import Hex.Stage.Build.BoxElem qualified as BoxElem
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as E
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hex.Stage.Parse.Interface.AST.Quantity qualified as Uneval
import Hex.Stage.Render.Interface.DocInstruction qualified as DVI
import Hexlude

-- What's the plan here?
-- I want to do as much evaluation as possible in this stage, but I'm not sure
-- what, if any, evaluation I need to defer to the interpretation stage, when
-- I'll have extra context about the current state of hex-lists and hex-boxes.
-- So I don't want to *eagerly* write an evaluated form of the parsed AST, then
-- find out when writing the interpreter, that I need to undo some of that work.
-- So my plan is to leave essentially all of the AST unchanged, then every time
-- I need to do some evaluation during interpretation, move that here, and
-- change the AST as needed. Then things continue to work and I don't have to
-- get things right all at once. I can leave some evaluation in the
-- interpretation stage while I'm unsure how to arrange things.

-- I've copied the top-level command type here, just to make the Parse/Evaluate
-- types distinct, so I don't confuse myself
data Command
  = ShowToken LT.LexToken
  | ShowBox Q.HexInt
  | ShowLists
  | ShowTheInternalQuantity Uneval.InternalQuantity
  | ShipOut Uneval.Box
  | AddMark HSt.TL.BalancedText
  | -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- then can 'migrate' out.
    AddInsertion Q.HexInt
  | AddAdjustment
  | AddSpace
  | StartParagraph ListExtractor.IndentFlag
  | EndParagraph
  | HModeCommand HModeCommand
  | VModeCommand VModeCommand
  | ModeIndependentCommand ModeIndependentCommand
  deriving stock (Show, Eq, Generic)

data Assignment = Assignment {body :: AssignmentBody, scope :: HSt.Grouped.ScopeFlag}
  deriving stock (Show, Eq, Generic)

data ModeIndependentCommand
  = Assign Assignment
  | Relax
  | IgnoreSpaces
  | AddPenalty Bad.FiniteBadnessVal
  | AddKern BoxElem.Kern
  | AddMathKern Q.MathLength
  | RemoveItem PT.RemovableItem
  | SetAfterAssignmentToken LT.LexToken
  | AddToAfterGroupTokens LT.LexToken
  | WriteMessage MessageWriteCommand
  | ModifyFileStream Uneval.FileStreamModificationCommand
  | WriteToStream StreamWriteCommand
  | DoSpecial HSt.TL.BalancedText
  | AddBox Uneval.BoxPlacement Box
  | ChangeScope Q.Sign HSt.Group.ChangeGroupTrigger
  | DebugShowState
  deriving stock (Show, Eq, Generic)

data HModeCommand
  = AddControlSpace
  | AddCharacter Code.CharCode
  | AddAccentedCharacter Uneval.HexInt [Assignment] (Maybe Uneval.CharCodeRef)
  | AddItalicCorrection
  | AddDiscretionaryText Uneval.DiscretionaryText
  | AddDiscretionaryHyphen
  | EnterMathMode
  | AddHGlue Q.Glue
  | AddHLeaders Uneval.LeadersSpec
  | AddHRule Box.Rule
  | AddVAlignedMaterial BoxSpecification
  | AddUnwrappedFetchedHBox Uneval.FetchedBoxRef -- \unh{box,copy}
  deriving stock (Show, Eq, Generic)

data VModeCommand
  = End
  | Dump
  | EnterHMode
  | AddVGlue Q.Glue
  | AddVLeaders Uneval.LeadersSpec
  | AddVRule Box.Rule
  | AddHAlignedMaterial BoxSpecification
  | AddUnwrappedFetchedVBox Uneval.FetchedBoxRef -- \unv{box,copy}
  deriving stock (Show, Eq, Generic)

data MessageWriteCommand = MessageWriteCommand {messageDest :: PT.StandardOutputInput, messageContents :: Text}
  deriving stock (Show, Eq, Generic)

data StreamWriteCommand = StreamWriteCommand {streamNumber :: Q.HexInt, writeText :: WriteText}
  deriving stock (Show, Eq, Generic)

data WriteText
  = ImmediateWriteText Text
  | DeferredWriteText HSt.TL.BalancedText
  deriving stock (Show, Eq, Generic)

data AssignmentBody
  = DefineControlSequence Res.ControlSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont DVI.FontNumber
  | SetFamilyMember HSt.Font.FamilyMember DVI.FontNumber
  | SetParShape Uneval.HexInt [Uneval.Length]
  | SetBoxRegister HSt.Register.RegisterLocation Box
  | -- GlobalScope assignments.
    SetFontDimension Uneval.FontDimensionRef Uneval.Length
  | SetFontSpecialChar E.FontSpecialCharRef Q.HexInt
  | SetHyphenation [HSt.Hyph.HyphenationException]
  | SetHyphenationPatterns [HSt.Hyph.HyphenationPattern]
  | SetBoxDimension Uneval.BoxDimensionRef Uneval.Length
  | SetInteractionMode PT.InteractionMode
  deriving stock (Show, Eq, Generic)

data CodeAssignment = CodeAssignment {codeIndex :: Code.CharCode, codeValue :: CodeValue}
  deriving stock (Show, Eq, Generic)

data CodeValue
  = CatCodeValue Code.CatCode
  | MathCodeValue Code.MathCode
  | UpperCaseCodeValue Code.UpperCaseCode
  | LowerCaseCodeValue Code.LowerCaseCode
  | SpaceFactorCodeValue Code.SpaceFactorCode
  | DelimiterCodeValue Code.DelimiterCode
  deriving stock (Show, Eq, Generic)

data ControlSequenceTarget
  = MacroTarget ST.MacroDefinition
  | LetTarget LT.LexToken
  | FutureLetTarget Uneval.FutureLetDefinition
  | ShortDefineTarget PT.ShortDefTargetValue
  | ReadTarget Q.HexInt
  | FontTarget FontFileSpec
  deriving stock (Show, Eq, Generic)

data FontFileSpec = FontFileSpec TFM.FontSpecification HexFilePath
  deriving stock (Show, Eq, Generic)

data Box
  = FetchedRegisterBox HSt.Register.BoxFetchMode HSt.Register.RegisterLocation
  | LastBox
  | VSplitBox Q.HexInt Q.Length
  | ExplicitBox BoxSpecification PT.ExplicitBoxType
  deriving stock (Show, Eq, Generic)

data BoxSpecification
  = Natural
  | To Q.Length
  | Spread Q.Length
  deriving stock (Show, Eq, Generic)

data VariableAssignment
  = IntVariableAssignment (QuantVariableAssignment 'Q.IntQuantity)
  | LengthVariableAssignment (QuantVariableAssignment 'Q.LengthQuantity)
  | GlueVariableAssignment (QuantVariableAssignment 'Q.GlueQuantity)
  | MathGlueVariableAssignment (QuantVariableAssignment 'Q.MathGlueQuantity)
  | TokenListVariableAssignment (QuantVariableAssignment 'Q.TokenListQuantity)
  | SpecialIntParameterVariableAssignment HSt.Param.SpecialIntParameter Q.HexInt
  | SpecialLengthParameterVariableAssignment HSt.Param.SpecialLengthParameter Q.Length
  deriving stock (Show, Eq, Generic)

data QuantVariableAssignment (q :: Q.QuantityType) = QuantVariableAssignment (HSt.Var.QuantVariable q) (HSt.Var.QuantVariableTarget q)
  deriving stock (Generic)

deriving stock instance (Show (HSt.Var.QuantVariable a), Show (HSt.Var.QuantVariableTarget a)) => Show (QuantVariableAssignment a)

deriving stock instance (Eq (HSt.Var.QuantVariable a), Eq (HSt.Var.QuantVariableTarget a)) => Eq (QuantVariableAssignment a)

data VariableModification
  = AdvanceIntVariable (HSt.Var.QuantVariable 'Q.IntQuantity) (HSt.Var.QuantVariableTarget 'Q.IntQuantity)
  | AdvanceLengthVariable (HSt.Var.QuantVariable 'Q.LengthQuantity) (HSt.Var.QuantVariableTarget 'Q.LengthQuantity)
  | AdvanceGlueVariable (HSt.Var.QuantVariable 'Q.GlueQuantity) (HSt.Var.QuantVariableTarget 'Q.GlueQuantity)
  | AdvanceMathGlueVariable (HSt.Var.QuantVariable 'Q.MathGlueQuantity) (HSt.Var.QuantVariableTarget 'Q.MathGlueQuantity)
  | ScaleVariable VDirection NumericVariable Q.HexInt
  deriving stock (Show, Eq, Generic)

data NumericVariable
  = IntNumericVariable (HSt.Var.QuantVariable 'Q.IntQuantity)
  | LengthNumericVariable (HSt.Var.QuantVariable 'Q.LengthQuantity)
  | GlueNumericVariable (HSt.Var.QuantVariable 'Q.GlueQuantity)
  | MathGlueNumericVariable (HSt.Var.QuantVariable 'Q.MathGlueQuantity)
  deriving stock (Show, Eq, Generic)
