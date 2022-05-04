{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Interface.AST.Command where

import Hex.Common.Codes qualified as H.Code
import Hex.Stage.Parse.Interface.AST.Quantity
import Hex.Common.Quantity qualified as Q
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as Res.PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as Res.ST
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
  | AddMark Res.ST.ExpandedBalancedText
  | -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- -- then can 'migrate' out.
    -- \| AddInsertion HexInt VModeMaterial
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
  | AddPenalty HexInt
  | AddKern Length
  | AddMathKern MathLength
  | RemoveItem Res.PT.RemovableItem
  | SetAfterAssignmentToken Lex.LexToken
  | AddToAfterGroupTokens Lex.LexToken
  | WriteMessage MessageWriteCommand
  | ModifyFileStream FileStreamModificationCommand
  | WriteToStream StreamWriteCommand
  | DoSpecial Res.ST.ExpandedBalancedText
  | AddBox BoxPlacement Box
  | ChangeScope Q.Sign CommandTrigger
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

data MessageWriteCommand = MessageWriteCommand Res.PT.StandardOutputSource Res.ST.ExpandedBalancedText
  deriving stock (Show, Eq, Generic)

data FileStreamModificationCommand = FileStreamModificationCommand FileStreamType FileStreamAction HexInt
  deriving stock (Show, Eq, Generic)

data Assignment = Assignment {body :: AssignmentBody, scope :: Res.PT.ScopeFlag}
  deriving stock (Show, Eq, Generic)

data ControlSequenceTarget
  = MacroTarget Res.ST.MacroDefinition
  | LetTarget Lex.LexToken
  | FutureLetTarget FutureLetDefinition
  | ShortDefineTarget Res.PT.CharryQuantityType HexInt
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
  | SelectFont Res.PT.FontNumber
  | SetFamilyMember FamilyMember FontRef
  | SetParShape HexInt [Length]
  | SetBoxRegister HexInt Box
  | -- -- Global assignments.
    SetFontDimension FontDimensionRef Length
  | SetFontChar FontCharRef HexInt
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
  QuantVariableTarget 'Res.PT.IntQuantity = HexInt
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
  | SpecialIntParameterVariableAssignment Res.PT.SpecialIntParameter HexInt
  | SpecialLengthParameterVariableAssignment Res.PT.SpecialLengthParameter Length
  deriving stock (Show, Eq, Generic)

data VariableModification
  = AdvanceIntVariable (QuantVariableAST 'Res.PT.IntQuantity) (QuantVariableTarget 'Res.PT.IntQuantity)
  | AdvanceLengthVariable (QuantVariableAST 'Res.PT.LenQuantity) (QuantVariableTarget 'Res.PT.LenQuantity)
  | AdvanceGlueVariable (QuantVariableAST 'Res.PT.GlueQuantity) (QuantVariableTarget 'Res.PT.GlueQuantity)
  | AdvanceMathGlueVariable (QuantVariableAST 'Res.PT.MathGlueQuantity) (QuantVariableTarget 'Res.PT.MathGlueQuantity)
  | ScaleVariable Q.VDirection NumericVariable HexInt
  deriving stock (Show, Eq, Generic)

data NumericVariable
  = IntNumericVariable (QuantVariableAST 'Res.PT.IntQuantity)
  | LengthNumericVariable (QuantVariableAST 'Res.PT.LenQuantity)
  | GlueNumericVariable (QuantVariableAST 'Res.PT.GlueQuantity)
  | MathGlueNumericVariable (QuantVariableAST 'Res.PT.MathGlueQuantity)
  deriving stock (Show, Eq, Generic)

data CodeAssignment = CodeAssignment {codeTableRef :: CodeTableRef, codeValue :: HexInt }
  deriving stock (Show, Eq, Generic)

data FontSpecification = NaturalFont | FontAt Length | FontScaled HexInt
  deriving stock (Show, Eq, Generic)

-- Box specification.
data Box
  = FetchedRegisterBox Res.PT.BoxFetchMode HexInt
  | LastBox
  | VSplitBox HexInt Length
  | ExplicitBox BoxSpecification Res.PT.ExplicitBox
  deriving stock (Show, Eq, Generic)

data BoxSpecification = Natural | To Length | Spread Length
  deriving stock (Show, Eq, Generic)

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule Q.Axis Rule
  deriving stock (Show, Eq, Generic)

data DiscretionaryText = DiscretionaryText {preBreak, postBreak, noBreak :: Res.ST.ExpandedBalancedText}
  deriving stock (Show, Eq, Generic)

data FetchedBoxRef = FetchedBoxRef HexInt Res.PT.BoxFetchMode
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

newtype Rule = Rule (Seq (Q.BoxDim, Length))
  deriving stock (Show, Eq, Generic)

data FileStreamAction = Open HexFilePath | Close
  deriving stock (Show, Eq, Generic)

data FileStreamType = FileInput | FileOutput WritePolicy
  deriving stock (Show, Eq, Generic)

data BoxPlacement = NaturalPlacement | ShiftedPlacement Q.Axis Q.Direction Length
  deriving stock (Show, Eq, Generic)

data CharCodeRef
  = CharRef H.Code.CharCode
  | CharTokenRef Q.HexInt
  | CharCodeNrRef CharCodeInt
  deriving stock (Show, Eq, Generic)
