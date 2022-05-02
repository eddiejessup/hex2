{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Evaluate.Interface.AST.Command where

import Hex.Common.Codes qualified as H.Code
import Hex.Stage.Evaluate.Interface.AST.Common
import Hex.Stage.Interpret.Build.Box.Elem qualified as H.Inter.B.Box
import Hex.Stage.Interpret.Build.List.Elem qualified as H.Inter.B.List
import Hex.Common.Quantity qualified as H.Q
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as Res.PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as Res.ST
import Hexlude
import Hex.Common.HexState.Interface.Resolve (ResolvedToken)
import qualified Hex.Stage.Lex.Interface.Extract as Lex

data Command
  = ShowToken Lex.LexToken
  | ShowBox H.Q.HexInt
  | ShowLists
  | ShowTheInternalQuantity InternalQuantity
  | ShipOut Box
  | AddMark Res.ST.ExpandedBalancedText
  | -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- -- then can 'migrate' out.
    -- \| AddInsertion H.Q.HexInt VModeMaterial
    -- \| AddAdjustment VModeMaterial
    AddSpace
  | StartParagraph Res.PT.IndentFlag
  | EndParagraph
  | -- \| AddAlignedMaterial DesiredLength AlignmentMaterial
    HModeCommand HModeCommand
  | VModeCommand VModeCommand
  | ModeIndependentCommand ModeIndependentCommand
  deriving stock (Show, Eq, Generic)

data ModeIndependentCommand
  = Assign Assignment
  | Relax
  | IgnoreSpaces
  | AddPenalty H.Inter.B.List.Penalty
  | AddKern H.Inter.B.Box.Kern
  | AddMathKern MathLength
  | RemoveItem Res.PT.RemovableItem
  | SetAfterAssignmentToken Lex.LexToken
  | AddToAfterGroupTokens Lex.LexToken
  | WriteMessage MessageWriteCommand
  | ModifyFileStream FileStreamModificationCommand
  | WriteToStream StreamWriteCommand
  | DoSpecial Res.ST.ExpandedBalancedText
  | AddBox BoxPlacement Box
  | ChangeScope H.Q.Sign CommandTrigger
  deriving stock (Show, Eq, Generic)

data VModeCommand
  = End
  | Dump
  | EnterHMode
  | AddVGlue H.Q.Glue
  | AddVLeaders LeadersSpec
  | AddVRule H.Inter.B.Box.Rule
  | AddUnwrappedFetchedVBox FetchedBoxRef -- \unv{box,copy}
  deriving stock (Show, Eq, Generic)

data HModeCommand
  = AddControlSpace
  | AddCharacter CharCodeRef
  | AddAccentedCharacter H.Q.HexInt [Assignment] (Maybe CharCodeRef)
  | AddItalicCorrection
  | AddDiscretionaryText DiscretionaryText
  | AddDiscretionaryHyphen
  | EnterMathMode
  | AddHGlue H.Q.Glue
  | AddHLeaders LeadersSpec
  | AddHRule H.Inter.B.Box.Rule
  | AddUnwrappedFetchedHBox FetchedBoxRef -- \unh{box,copy}
  deriving stock (Show, Eq, Generic)

data StreamWriteCommand = StreamWriteCommand H.Q.HexInt WriteText
  deriving stock (Show, Eq, Generic)

data MessageWriteCommand = MessageWriteCommand Res.PT.StandardOutputSource ByteString
  deriving stock (Show, Eq, Generic)

data FileStreamModificationCommand = FileStreamModificationCommand FileStreamType FileStreamAction H.Q.HexInt
  deriving stock (Show, Eq, Generic)

data Assignment = Assignment {body :: AssignmentBody, scope :: Res.PT.ScopeFlag}
  deriving stock (Show, Eq, Generic)

data ControlSequenceTarget
  = NonFontTarget ResolvedToken
  | FontTarget FontFileSpec
  deriving stock (Show, Eq, Generic)

data FontFileSpec = FontFileSpec H.Inter.B.Box.FontSpecification H.Inter.B.Box.HexFilePath
  deriving stock (Show, Eq, Generic)

data AssignmentBody
  = DefineControlSequence Res.ControlSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont H.Q.HexInt
  | SetFamilyMember FamilyMember FontRef
  | SetParShape H.Q.HexInt [Length]
  | SetBoxRegister H.Q.HexInt Box
  | -- -- Global assignments.
    SetFontDimension FontDimensionRef Length
  | SetFontChar FontCharRef H.Q.HexInt
  | SetHyphenation Res.ST.InhibitedBalancedText
  | SetHyphenationPatterns Res.ST.InhibitedBalancedText
  | SetBoxDimension BoxDimensionRef Length
  | SetInteractionMode Res.PT.InteractionMode
  deriving stock (Show, Eq, Generic)

data TokenListAssignmentTarget
  = TokenListAssignmentVar (QuantVariableAST 'Res.PT.TokenListQuantity)
  | TokenListAssignmentText Res.ST.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data QuantVariableAssignment (q :: Res.PT.QuantityType) = QuantVariableAssignment (QuantVariableAST q) (QuantVariableTarget q)
  deriving stock (Generic)

deriving stock instance (Show (QuantVariableAST a), Show (QuantVariableTarget a)) => Show (QuantVariableAssignment a)

deriving stock instance (Eq (QuantVariableAST a), Eq (QuantVariableTarget a)) => Eq (QuantVariableAssignment a)

type family QuantVariableTarget a where
  QuantVariableTarget 'Res.PT.IntQuantity = H.Q.HexInt
  QuantVariableTarget 'Res.PT.LenQuantity = Length
  QuantVariableTarget 'Res.PT.GlueQuantity = Glue
  QuantVariableTarget 'Res.PT.MathGlueQuantity = MathGlue
  QuantVariableTarget 'Res.PT.TokenListQuantity = TokenListAssignmentTarget

data VariableAssignment
  = IntVariableAssignment (QuantVariableAssignment 'Res.PT.IntQuantity)
  | LengthVariableAssignment (QuantVariableAssignment 'Res.PT.LenQuantity)
  | GlueVariableAssignment (QuantVariableAssignment 'Res.PT.GlueQuantity)
  | MathGlueVariableAssignment (QuantVariableAssignment 'Res.PT.MathGlueQuantity)
  | TokenListVariableAssignment (QuantVariableAssignment 'Res.PT.TokenListQuantity)
  | SpecialIntParameterVariableAssignment Res.PT.SpecialIntParameter H.Q.HexInt
  | SpecialLengthParameterVariableAssignment Res.PT.SpecialLengthParameter Length
  deriving stock (Show, Eq, Generic)

data VariableModification
  = AdvanceIntVariable (QuantVariableAST 'Res.PT.IntQuantity) (QuantVariableTarget 'Res.PT.IntQuantity)
  | AdvanceLengthVariable (QuantVariableAST 'Res.PT.LenQuantity) (QuantVariableTarget 'Res.PT.LenQuantity)
  | AdvanceGlueVariable (QuantVariableAST 'Res.PT.GlueQuantity) (QuantVariableTarget 'Res.PT.GlueQuantity)
  | AdvanceMathGlueVariable (QuantVariableAST 'Res.PT.MathGlueQuantity) (QuantVariableTarget 'Res.PT.MathGlueQuantity)
  | ScaleVariable H.Q.VDirection NumericVariable H.Q.HexInt
  deriving stock (Show, Eq, Generic)

data NumericVariable
  = IntNumericVariable (QuantVariableAST 'Res.PT.IntQuantity)
  | LengthNumericVariable (QuantVariableAST 'Res.PT.LenQuantity)
  | GlueNumericVariable (QuantVariableAST 'Res.PT.GlueQuantity)
  | MathGlueNumericVariable (QuantVariableAST 'Res.PT.MathGlueQuantity)
  deriving stock (Show, Eq, Generic)

data CodeAssignment = CodeAssignment CodeTableRef H.Q.HexInt
  deriving stock (Show, Eq, Generic)

-- Box specification.
data Box
  = FetchedRegisterBox Res.PT.BoxFetchMode H.Q.HexInt
  | LastBox
  | VSplitBox H.Q.HexInt Length
  | ExplicitBox BoxSpecification Res.PT.ExplicitBox
  deriving stock (Show, Eq, Generic)

data BoxSpecification = Natural | To Length | Spread Length
  deriving stock (Show, Eq, Generic)

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule H.Q.Axis H.Inter.B.Box.Rule
  deriving stock (Show, Eq, Generic)

data DiscretionaryText = DiscretionaryText {preBreak, postBreak, noBreak :: Res.ST.ExpandedBalancedText}
  deriving stock (Show, Eq, Generic)

data FetchedBoxRef = FetchedBoxRef H.Q.HexInt Res.PT.BoxFetchMode
  deriving stock (Show, Eq, Generic)

data LeadersSpec = LeadersSpec Res.PT.LeadersType BoxOrRule Glue
  deriving stock (Show, Eq, Generic)

data CommandTrigger = CharCommandTrigger | CSCommandTrigger
  deriving stock (Show, Eq, Generic)

data InternalQuantity
  = InternalIntQuantity InternalInt
  | InternalLengthQuantity InternalLength
  | InternalGlueQuantity InternalGlue
  | InternalMathGlueQuantity InternalMathGlue
  | FontQuantity FontRef
  | TokenListVariableQuantity (QuantVariableAST 'Res.PT.TokenListQuantity)
  deriving stock (Show, Eq, Generic)

data WriteText
  = ImmediateWriteText Res.ST.ExpandedBalancedText
  | DeferredWriteText Res.ST.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data WritePolicy = Immediate | Deferred
  deriving stock (Show, Eq, Generic)

data FileStreamAction = Open H.Inter.B.Box.HexFilePath | Close
  deriving stock (Show, Eq, Generic)

data FileStreamType = FileInput | FileOutput WritePolicy
  deriving stock (Show, Eq, Generic)

data BoxPlacement = NaturalPlacement | ShiftedPlacement H.Q.Axis H.Q.Direction Length
  deriving stock (Show, Eq, Generic)

data CharCodeRef
  = CharRef H.Code.CharCode
  | CharTokenRef H.Q.HexInt
  | CharCodeNrRef H.Q.HexInt
  deriving stock (Show, Eq, Generic)
