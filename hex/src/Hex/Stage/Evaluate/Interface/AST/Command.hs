{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Evaluate.Interface.AST.Command where

import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.Font qualified as Font
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Group
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Hyphen qualified as HSt.Hyph
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
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
import Hexlude

data Command
  = ModeDependentCommand ModeDependentCommand
  | HModeCommand HModeCommand
  | VModeCommand VModeCommand
  | ModeIndependentCommand ModeIndependentCommand
  deriving stock (Show, Eq, Generic)

data ModeDependentCommand
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
  | ModifyFileStream FileStreamModificationCommand
  | WriteToStream StreamWriteCommand
  | DoSpecial HSt.TL.BalancedText
  | AddBox (Maybe (Uneval.OffsetAlongAxis Q.Length)) Box
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
  | AddHRule (Box.BoxDims Q.Length)
  | AddVAlignedMaterial BoxSpecification
  | AddUnwrappedFetchedHBox FetchedBoxRef -- \unh{box,copy}
  deriving stock (Show, Eq, Generic)

data VModeCommand
  = End
  | Dump
  | EnterHMode
  | AddVGlue Q.Glue
  | AddVLeaders Uneval.LeadersSpec
  | AddVRule (Box.BoxDims Q.Length)
  | AddHAlignedMaterial BoxSpecification
  | AddUnwrappedFetchedVBox FetchedBoxRef -- \unv{box,copy}
  deriving stock (Show, Eq, Generic)

data Assignment = Assignment {body :: AssignmentBody, scope :: HSt.Grouped.ScopeFlag}
  deriving stock (Show, Eq, Generic)

data MessageWriteCommand = MessageWriteCommand {messageDest :: PT.StandardOutputInput, messageContents :: Text}
  deriving stock (Show, Eq, Generic)

data StreamWriteCommand = StreamWriteCommand {streamNumber :: Q.HexInt, writeText :: WriteText}
  deriving stock (Show, Eq, Generic)

data WriteText
  = ImmediateWriteText Text
  | DeferredWriteText HSt.TL.BalancedText
  deriving stock (Show, Eq, Generic)

data FileStreamModificationCommand = FileStreamModificationCommand
  { fileStreamType :: Uneval.FileStreamType,
    fileStreamAction :: Uneval.FileStreamAction,
    fileStreamNr :: Q.FourBitInt
  }
  deriving stock (Show, Eq, Generic)

data AssignmentBody
  = DefineControlSequence Res.ControlSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont Font.FontNumber
  | SetFamilyMember HSt.Font.FamilyMember Font.FontNumber
  | SetParShape Uneval.HexInt [Uneval.Length]
  | SetBoxRegister HSt.Reg.RegisterLocation Box
  | -- GlobalScope assignments.
    SetFontDimension Uneval.FontDimensionRef Uneval.Length
  | SetFontSpecialChar E.FontSpecialCharRef Q.HexInt
  | SetHyphenation (Map HSt.Hyph.WordCodes HSt.Hyph.WordHyphenationPoints)
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
  = FetchedRegisterBox HSt.Reg.BoxFetchMode HSt.Reg.RegisterLocation
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

data FetchedBoxRef = FetchedBoxRef HSt.Reg.RegisterLocation HSt.Reg.BoxFetchMode
  deriving stock (Show, Eq, Generic)
