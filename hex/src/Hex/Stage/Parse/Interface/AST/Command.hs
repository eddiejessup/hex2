{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Interface.AST.Command where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Register
import Hex.Common.HexState.Interface.Resolve (ControlSymbol)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Interface.AST.Quantity
import Hexlude

data Command
  = ShowToken Lex.LexToken
  | ShowBox HexInt
  | ShowLists
  | ShowTheInternalQuantity InternalQuantity
  | ShipOut Box
  | AddMark ST.ExpandedBalancedText
  | -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- -- then can 'migrate' out.
    -- \| AddInsertion HexInt VModeMaterial
    -- \| AddAdjustment VModeMaterial
    AddSpace
  | StartParagraph ListExtractor.IndentFlag
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
  | AddPenalty HexInt
  | AddKern Length
  | AddMathKern MathLength
  | RemoveItem PT.RemovableItem
  | SetAfterAssignmentToken Lex.LexToken
  | AddToAfterGroupTokens Lex.LexToken
  | WriteMessage MessageWriteCommand
  | ModifyFileStream FileStreamModificationCommand
  | WriteToStream StreamWriteCommand
  | DoSpecial ST.ExpandedBalancedText
  | AddBox BoxPlacement Box
  | ChangeScope Q.Sign HSt.Grouped.ChangeGroupTrigger
  | DebugShowState
  deriving stock (Show, Eq, Generic)

data VModeCommand
  = End
  | Dump
  | EnterHMode
  | AddVGlue Glue
  | AddVLeaders LeadersSpec
  | AddVRule Rule
  | AddUnwrappedFetchedVBox FetchedBoxRef -- \unv{box,copy}
  deriving stock (Show, Eq, Generic)

data HModeCommand
  = AddControlSpace
  | AddCharacter CharCodeRef
  | AddAccentedCharacter HexInt [Assignment] (Maybe CharCodeRef)
  | AddItalicCorrection
  | AddDiscretionaryText DiscretionaryText
  | AddDiscretionaryHyphen
  | EnterMathMode
  | AddHGlue Glue
  | AddHLeaders LeadersSpec
  | AddHRule Rule
  | AddUnwrappedFetchedHBox FetchedBoxRef -- \unh{box,copy}
  deriving stock (Show, Eq, Generic)

data StreamWriteCommand = StreamWriteCommand HexInt WriteText
  deriving stock (Show, Eq, Generic)

data MessageWriteCommand = MessageWriteCommand
  { messageDest :: PT.StandardOutputSource,
    messageContents :: ST.ExpandedBalancedText
  }
  deriving stock (Show, Eq, Generic)

data FileStreamModificationCommand = FileStreamModificationCommand FileStreamType FileStreamAction HexInt
  deriving stock (Show, Eq, Generic)

data Assignment = Assignment {body :: AssignmentBody, scope :: HSt.Grouped.ScopeFlag}
  deriving stock (Show, Eq, Generic)

data ControlSequenceTarget
  = MacroTarget ST.MacroDefinition
  | LetTarget Lex.LexToken
  | FutureLetTarget FutureLetDefinition
  | ShortDefineTarget PT.CharryQuantityType HexInt
  | ReadTarget HexInt
  | FontTarget FontFileSpec
  deriving stock (Show, Eq, Generic)

-- | \futurelet (token1) (token2) (token3)
-- (token1): Just the name of the control sequence we are defining, as with any
-- control-sequence definition.
-- (token3): The argument of the assignment, as in a normal 'let', i.e. (token1)
-- will be assigned the current meaning of (token3).
-- (token2): A token that we will expand immediately after performing the above
-- assignment. (i.e. at 'let' definition-time, not when the \let definition is
-- used.)
-- For an example of how this might be used, see:
-- https://tug.org/TUGboat/tb09-3/tb22bechtolsheim.pdf
data FutureLetDefinition = FutureLetTargetDefinition {tokenToExpand :: Lex.LexToken, letTargetToken :: Lex.LexToken}
  deriving stock (Show, Eq, Generic)

data FontFileSpec = FontFileSpec {fontSpec :: FontSpecification, fontPath :: Q.HexFilePath}
  deriving stock (Show, Eq, Generic)

data AssignmentBody
  = DefineControlSequence ControlSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont HSt.Font.FontNumber
  | SetFamilyMember FamilyMember FontRef
  | SetParShape HexInt [Length]
  | SetBoxRegister ExplicitRegisterLocation Box
  | -- -- Global assignments.
    SetFontDimension FontDimensionRef Length
  | SetFontSpecialChar FontSpecialCharRef HexInt
  | SetHyphenation ST.InhibitedBalancedText
  | SetHyphenationPatterns ST.InhibitedBalancedText
  | SetBoxDimension BoxDimensionRef Length
  | SetInteractionMode PT.InteractionMode
  deriving stock (Show, Eq, Generic)

data TokenListAssignmentTarget
  = TokenListAssignmentVar (QuantVariableAST 'Q.TokenListQuantity)
  | TokenListAssignmentText ST.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data QuantVariableAssignment (q :: Q.QuantityType) = QuantVariableAssignment (QuantVariableAST q) (QuantVariableTargetAST q)
  deriving stock (Generic)

deriving stock instance (Show (QuantVariableAST a), Show (QuantVariableTargetAST a)) => Show (QuantVariableAssignment a)

deriving stock instance (Eq (QuantVariableAST a), Eq (QuantVariableTargetAST a)) => Eq (QuantVariableAssignment a)

type family QuantVariableTargetAST a where
  QuantVariableTargetAST 'Q.IntQuantity = HexInt
  QuantVariableTargetAST 'Q.LengthQuantity = Length
  QuantVariableTargetAST 'Q.GlueQuantity = Glue
  QuantVariableTargetAST 'Q.MathGlueQuantity = MathGlue
  QuantVariableTargetAST 'Q.TokenListQuantity = TokenListAssignmentTarget

data VariableAssignment
  = IntVariableAssignment (QuantVariableAssignment 'Q.IntQuantity)
  | LengthVariableAssignment (QuantVariableAssignment 'Q.LengthQuantity)
  | GlueVariableAssignment (QuantVariableAssignment 'Q.GlueQuantity)
  | MathGlueVariableAssignment (QuantVariableAssignment 'Q.MathGlueQuantity)
  | TokenListVariableAssignment (QuantVariableAssignment 'Q.TokenListQuantity)
  | SpecialIntParameterVariableAssignment HSt.Param.SpecialIntParameter HexInt
  | SpecialLengthParameterVariableAssignment HSt.Param.SpecialLengthParameter Length
  deriving stock (Show, Eq, Generic)

data VariableModification
  = AdvanceIntVariable (QuantVariableAST 'Q.IntQuantity) (QuantVariableTargetAST 'Q.IntQuantity)
  | AdvanceLengthVariable (QuantVariableAST 'Q.LengthQuantity) (QuantVariableTargetAST 'Q.LengthQuantity)
  | AdvanceGlueVariable (QuantVariableAST 'Q.GlueQuantity) (QuantVariableTargetAST 'Q.GlueQuantity)
  | AdvanceMathGlueVariable (QuantVariableAST 'Q.MathGlueQuantity) (QuantVariableTargetAST 'Q.MathGlueQuantity)
  | ScaleVariable Q.VDirection NumericVariable HexInt
  deriving stock (Show, Eq, Generic)

data NumericVariable
  = IntNumericVariable (QuantVariableAST 'Q.IntQuantity)
  | LengthNumericVariable (QuantVariableAST 'Q.LengthQuantity)
  | GlueNumericVariable (QuantVariableAST 'Q.GlueQuantity)
  | MathGlueNumericVariable (QuantVariableAST 'Q.MathGlueQuantity)
  deriving stock (Show, Eq, Generic)

data CodeAssignment = CodeAssignment {codeTableRef :: CodeTableRef, codeValue :: HexInt}
  deriving stock (Show, Eq, Generic)

data FontSpecification = NaturalFont | FontAt Length | FontScaled HexInt
  deriving stock (Show, Eq, Generic)

-- Box specification.
data Box
  = FetchedRegisterBox HSt.Register.BoxFetchMode ExplicitRegisterLocation
  | LastBox
  | VSplitBox HexInt Length
  | ExplicitBox BoxSpecification PT.ExplicitBoxType
  deriving stock (Show, Eq, Generic)

data BoxSpecification
  = Natural
  | To Length
  | Spread Length
  deriving stock (Show, Eq, Generic)

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule Q.Axis Rule
  deriving stock (Show, Eq, Generic)

data DiscretionaryText = DiscretionaryText {preBreak, postBreak, noBreak :: ST.ExpandedBalancedText}
  deriving stock (Show, Eq, Generic)

data FetchedBoxRef = FetchedBoxRef HexInt HSt.Register.BoxFetchMode
  deriving stock (Show, Eq, Generic)

data LeadersSpec = LeadersSpec PT.LeadersType BoxOrRule Glue
  deriving stock (Show, Eq, Generic)

data InternalQuantity
  = InternalIntQuantity InternalInt
  | InternalLengthQuantity InternalLength
  | InternalGlueQuantity InternalGlue
  | InternalMathGlueQuantity InternalMathGlue
  | FontQuantity FontRef
  | TokenListVariableQuantity (QuantVariableAST 'Q.TokenListQuantity)
  deriving stock (Show, Eq, Generic)

data WriteText
  = ImmediateWriteText ST.ExpandedBalancedText
  | DeferredWriteText ST.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data WritePolicy = Immediate | Deferred
  deriving stock (Show, Eq, Generic)

newtype Rule = Rule (Seq (Q.BoxDim, Length))
  deriving stock (Show, Eq, Generic)

data FileStreamAction = Open Q.HexFilePath | Close
  deriving stock (Show, Eq, Generic)

data FileStreamType = FileInput | FileOutput WritePolicy
  deriving stock (Show, Eq, Generic)

data BoxPlacement = NaturalPlacement | ShiftedPlacement Q.Axis Q.Direction Length
  deriving stock (Show, Eq, Generic)

data CharCodeRef
  = CharRef Code.CharCode
  | CharTokenRef Q.HexInt
  | CharCodeNrRef CharCodeInt
  deriving stock (Show, Eq, Generic)
