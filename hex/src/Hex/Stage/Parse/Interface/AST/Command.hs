{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Interface.AST.Command where

import Hex.Common.Codes qualified as Code
import Hex.Stage.Parse.Interface.AST.Quantity
import Hex.Common.Quantity qualified as Q
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hexlude
import qualified Hex.Stage.Lex.Interface.Extract as Lex
import Hex.Common.HexState.Interface.Resolve (ControlSymbol)
import Hex.Stage.Interpret.Build.Box.Elem (HexFilePath)

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
  | StartParagraph PT.IndentFlag
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
  | ChangeScope Q.Sign CommandTrigger
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

data MessageWriteCommand = MessageWriteCommand { messageDest :: PT.StandardOutputSource, messageContents :: ST.ExpandedBalancedText}
  deriving stock (Show, Eq, Generic)

data FileStreamModificationCommand = FileStreamModificationCommand FileStreamType FileStreamAction HexInt
  deriving stock (Show, Eq, Generic)

data Assignment = Assignment {body :: AssignmentBody, scope :: PT.ScopeFlag}
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
data FutureLetDefinition = FutureLetTargetDefinition { tokenToExpand :: Lex.LexToken, letTargetToken :: Lex.LexToken }
  deriving stock (Show, Eq, Generic)

data FontFileSpec = FontFileSpec { fontSpec :: FontSpecification, fontPath :: HexFilePath }
  deriving stock (Show, Eq, Generic)

data AssignmentBody
  = DefineControlSequence ControlSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont PT.FontNumber
  | SetFamilyMember FamilyMember FontRef
  | SetParShape HexInt [Length]
  | SetBoxRegister HexInt Box
  | -- -- Global assignments.
    SetFontDimension FontDimensionRef Length
  | SetFontChar FontCharRef HexInt
  | SetHyphenation ST.InhibitedBalancedText
  | SetHyphenationPatterns ST.InhibitedBalancedText
  | SetBoxDimension BoxDimensionRef Length
  | SetInteractionMode PT.InteractionMode
  deriving stock (Show, Eq, Generic)

data TokenListAssignmentTarget
  = TokenListAssignmentVar (QuantVariableAST 'PT.TokenListQuantity)
  | TokenListAssignmentText ST.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data QuantVariableAssignment (q :: PT.QuantityType) = QuantVariableAssignment (QuantVariableAST q) (QuantVariableTargetAST q)
  deriving stock (Generic)

deriving stock instance (Show (QuantVariableAST a), Show (QuantVariableTargetAST a)) => Show (QuantVariableAssignment a)

deriving stock instance (Eq (QuantVariableAST a), Eq (QuantVariableTargetAST a)) => Eq (QuantVariableAssignment a)

type family QuantVariableTargetAST a where
  QuantVariableTargetAST 'PT.IntQuantity = HexInt
  QuantVariableTargetAST 'PT.LengthQuantity = Length
  QuantVariableTargetAST 'PT.GlueQuantity = Glue
  QuantVariableTargetAST 'PT.MathGlueQuantity = MathGlue
  QuantVariableTargetAST 'PT.TokenListQuantity = TokenListAssignmentTarget

data VariableAssignment
  = IntVariableAssignment (QuantVariableAssignment 'PT.IntQuantity)
  | LengthVariableAssignment (QuantVariableAssignment 'PT.LengthQuantity)
  | GlueVariableAssignment (QuantVariableAssignment 'PT.GlueQuantity)
  | MathGlueVariableAssignment (QuantVariableAssignment 'PT.MathGlueQuantity)
  | TokenListVariableAssignment (QuantVariableAssignment 'PT.TokenListQuantity)
  | SpecialIntParameterVariableAssignment PT.SpecialIntParameter HexInt
  | SpecialLengthParameterVariableAssignment PT.SpecialLengthParameter Length
  deriving stock (Show, Eq, Generic)

data VariableModification
  = AdvanceIntVariable (QuantVariableAST 'PT.IntQuantity) (QuantVariableTargetAST 'PT.IntQuantity)
  | AdvanceLengthVariable (QuantVariableAST 'PT.LengthQuantity) (QuantVariableTargetAST 'PT.LengthQuantity)
  | AdvanceGlueVariable (QuantVariableAST 'PT.GlueQuantity) (QuantVariableTargetAST 'PT.GlueQuantity)
  | AdvanceMathGlueVariable (QuantVariableAST 'PT.MathGlueQuantity) (QuantVariableTargetAST 'PT.MathGlueQuantity)
  | ScaleVariable Q.VDirection NumericVariable HexInt
  deriving stock (Show, Eq, Generic)

data NumericVariable
  = IntNumericVariable (QuantVariableAST 'PT.IntQuantity)
  | LengthNumericVariable (QuantVariableAST 'PT.LengthQuantity)
  | GlueNumericVariable (QuantVariableAST 'PT.GlueQuantity)
  | MathGlueNumericVariable (QuantVariableAST 'PT.MathGlueQuantity)
  deriving stock (Show, Eq, Generic)

data CodeAssignment = CodeAssignment {codeTableRef :: CodeTableRef, codeValue :: HexInt }
  deriving stock (Show, Eq, Generic)

data FontSpecification = NaturalFont | FontAt Length | FontScaled HexInt
  deriving stock (Show, Eq, Generic)

-- Box specification.
data Box
  = FetchedRegisterBox PT.BoxFetchMode HexInt
  | LastBox
  | VSplitBox HexInt Length
  | ExplicitBox BoxSpecification PT.ExplicitBox
  deriving stock (Show, Eq, Generic)

data BoxSpecification = Natural | To Length | Spread Length
  deriving stock (Show, Eq, Generic)

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule Q.Axis Rule
  deriving stock (Show, Eq, Generic)

data DiscretionaryText = DiscretionaryText {preBreak, postBreak, noBreak :: ST.ExpandedBalancedText}
  deriving stock (Show, Eq, Generic)

data FetchedBoxRef = FetchedBoxRef HexInt PT.BoxFetchMode
  deriving stock (Show, Eq, Generic)

data LeadersSpec = LeadersSpec PT.LeadersType BoxOrRule Glue
  deriving stock (Show, Eq, Generic)

data CommandTrigger = CharCommandTrigger | CSCommandTrigger
  deriving stock (Show, Eq, Generic)

data InternalQuantity
  = InternalIntQuantity InternalInt
  | InternalLengthQuantity InternalLength
  | InternalGlueQuantity InternalGlue
  | InternalMathGlueQuantity InternalMathGlue
  | FontQuantity FontRef
  | TokenListVariableQuantity (QuantVariableAST 'PT.TokenListQuantity)
  deriving stock (Show, Eq, Generic)

data WriteText
  = ImmediateWriteText ST.ExpandedBalancedText
  | DeferredWriteText ST.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data WritePolicy = Immediate | Deferred
  deriving stock (Show, Eq, Generic)

newtype Rule = Rule (Seq (Q.BoxDim, Length))
  deriving stock (Show, Eq, Generic)

data FileStreamAction = Open HexFilePath | Close
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
