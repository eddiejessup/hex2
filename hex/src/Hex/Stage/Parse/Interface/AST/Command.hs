{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Interface.AST.Command where

import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.Font qualified as Font
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Hyphen qualified as HSt.Hyph
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Register
import Hex.Common.HexState.Interface.Resolve (ControlSymbol)
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Expandable qualified as ST
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hex.Stage.Parse.Interface.AST.Quantity
import Hexlude

data Command
  = ShowToken LT.LexToken
  | ShowBox HexInt
  | ShowLists
  | ShowTheInternalQuantity InternalQuantity
  | ShipOut Box
  | AddMark HSt.TL.BalancedText
  | -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- then can 'migrate' out.
    AddInsertion HexInt
  | AddAdjustment
  | AddSpace
  | StartParagraph ListExtractor.IndentFlag
  | EndParagraph
  | HModeCommand HModeCommand
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
  | SetAfterAssignmentToken LT.LexToken
  | AddToAfterGroupTokens LT.LexToken
  | WriteMessage MessageWriteCommand
  | ModifyFileStream FileStreamModificationCommand
  | WriteToStream StreamWriteCommand
  | DoSpecial HSt.TL.BalancedText
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
  | AddHAlignedMaterial BoxSpecification
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
  | AddVAlignedMaterial BoxSpecification
  | AddUnwrappedFetchedHBox FetchedBoxRef -- \unh{box,copy}
  deriving stock (Show, Eq, Generic)

data StreamWriteCommand = StreamWriteCommand HexInt WriteText
  deriving stock (Show, Eq, Generic)

data MessageWriteCommand = MessageWriteCommand
  { messageDest :: PT.StandardOutputInput,
    messageContents :: HSt.TL.BalancedText
  }
  deriving stock (Show, Eq, Generic)

data FileStreamModificationCommand = FileStreamModificationCommand FileStreamType FileStreamAction HexInt
  deriving stock (Show, Eq, Generic)

data Assignment = Assignment {body :: AssignmentBody, scope :: HSt.Grouped.ScopeFlag}
  deriving stock (Show, Eq, Generic)

data ShortDefTargetValue = ShortDefTargetValue PT.CharryQuantityType HexInt
  deriving stock (Show, Eq, Generic)

data ControlSequenceTarget
  = MacroTarget ST.MacroDefinition
  | LetTarget LT.LexToken
  | FutureLetTarget FutureLetDefinition
  | ShortDefineTarget ShortDefTargetValue
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
data FutureLetDefinition = FutureLetTargetDefinition {tokenToExpand :: LT.LexToken, letTargetToken :: LT.LexToken}
  deriving stock (Show, Eq, Generic)

data FontFileSpec = FontFileSpec {fontSpec :: FontSpecification, fontPath :: HexFilePath}
  deriving stock (Show, Eq, Generic)

data AssignmentBody
  = DefineControlSequence ControlSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont Font.FontNumber
  | SetFamilyMember FamilyMember FontRef
  | SetParShape HexInt [Length]
  | SetBoxRegister ExplicitRegisterLocation Box
  | -- GlobalScope assignments.
    SetFontDimension FontDimensionRef Length
  | SetFontSpecialChar FontSpecialCharRef HexInt
  | SetHyphenation [HyphenationException]
  | SetHyphenationPatterns [HSt.Hyph.HyphenationPattern]
  | SetBoxDimension BoxDimensionRef Length
  | SetInteractionMode PT.InteractionMode
  deriving stock (Show, Eq, Generic)

-- `Nothing` represents a hyphen.
newtype HyphenationException
  = HyphenationException (NonEmpty (Maybe Code.CharCode))
  deriving stock (Show, Eq, Generic)

data TokenListAssignmentTarget
  = TokenListAssignmentVar (QuantVariableAST 'Q.TokenListQuantity)
  | TokenListAssignmentText HSt.TL.BalancedText
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
  | ScaleVariable VDirection NumericVariable HexInt
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

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule Axis Rule
  deriving stock (Show, Eq, Generic)

data DiscretionaryText = DiscretionaryText {preBreak, postBreak, noBreak :: HSt.TL.BalancedText}
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
  = ImmediateWriteText HSt.TL.BalancedText
  | DeferredWriteText HSt.TL.BalancedText
  deriving stock (Show, Eq, Generic)

data WritePolicy = Immediate | Deferred
  deriving stock (Show, Eq, Generic)

newtype Rule = Rule (Seq (Box.BoxDim, Length))
  deriving stock (Show, Eq, Generic)

data FileStreamAction = Open HexFilePath | Close
  deriving stock (Show, Eq, Generic)

data FileStreamType = FileInput | FileOutput WritePolicy
  deriving stock (Show, Eq, Generic)

data BoxPlacement = NaturalPlacement | ShiftedPlacement Axis Direction Length
  deriving stock (Show, Eq, Generic)

data CharCodeRef
  = CharRef Code.CharCode
  | CharTokenRef Q.HexInt
  | CharCodeNrRef CharCodeInt
  deriving stock (Show, Eq, Generic)
