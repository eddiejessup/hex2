{-# LANGUAGE UndecidableInstances #-}
module Hex.Parse.AST.Command where

import Hex.Codes qualified as H.Code
import Hex.Lex.Types qualified as H.Lex
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hexlude
import qualified Hex.Symbol.Types as H.Sym
import Hex.Parse.AST.Common

data Command
  = ShowToken H.Lex.LexToken
  | ShowBox HexInt
  | ShowLists
  | ShowTheInternalQuantity InternalQuantity
  | ShipOut Box
  | AddMark H.Sym.Tok.ExpandedBalancedText
  | -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- -- then can 'migrate' out.
    -- \| AddInsertion HexInt VModeMaterial
    -- \| AddAdjustment VModeMaterial
    AddSpace
  | StartParagraph H.Sym.Tok.IndentFlag
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
  | RemoveItem H.Sym.Tok.RemovableItem
  | SetAfterAssignmentToken H.Lex.LexToken
  | AddToAfterGroupTokens H.Lex.LexToken
  | WriteMessage MessageWriteCommand
  | ModifyFileStream FileStreamModificationCommand
  | WriteToStream StreamWriteCommand
  | DoSpecial H.Sym.Tok.ExpandedBalancedText
  | AddBox BoxPlacement Box
  | ChangeScope H.Q.Sign CommandTrigger
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

data MessageWriteCommand = MessageWriteCommand H.Sym.Tok.StandardOutputStream H.Sym.Tok.ExpandedBalancedText
  deriving stock (Show, Eq, Generic)

data FileStreamModificationCommand = FileStreamModificationCommand FileStreamType FileStreamAction HexInt
  deriving stock (Show, Eq, Generic)

data Assignment = Assignment {body :: AssignmentBody, scope :: H.Sym.Tok.ScopeFlag}
  deriving stock (Show, Eq, Generic)

newtype HexFilePath = HexFilePath FilePath
  deriving stock (Show, Eq, Generic)

data ControlSequenceTarget
  = MacroTarget H.Sym.Tok.MacroDefinition
  | LetTarget H.Lex.LexToken
  | FutureLetTarget H.Lex.LexToken H.Lex.LexToken
  | ShortDefineTarget H.Sym.Tok.CharryQuantityType HexInt
  | ReadTarget HexInt
  | FontTarget FontFileSpec
  deriving stock (Show, Eq, Generic)

data FontFileSpec = FontFileSpec FontSpecification HexFilePath
  deriving stock (Show, Eq, Generic)

data AssignmentBody
  = DefineControlSequence H.Sym.ControlSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont H.Q.HexInt
  | SetFamilyMember FamilyMember FontRef
  | SetParShape HexInt [Length]
  | SetBoxRegister HexInt Box
  | -- -- Global assignments.
    SetFontDimension FontDimensionRef Length
  | SetFontChar FontCharRef HexInt
  | SetHyphenation H.Sym.Tok.InhibitedBalancedText
  | SetHyphenationPatterns H.Sym.Tok.InhibitedBalancedText
  | SetBoxDimension BoxDimensionRef Length
  | SetInteractionMode H.Sym.Tok.InteractionMode
  deriving stock (Show, Eq, Generic)

data TokenListAssignmentTarget
  = TokenListAssignmentVar (QuantVariableAST 'H.Sym.Tok.TokenListQuantity)
  | TokenListAssignmentText H.Sym.Tok.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data QuantVariableAssignment (q :: H.Sym.Tok.QuantityType) = QuantVariableAssignment (QuantVariableAST q) (QuantVariableTarget q)
  deriving stock (Generic)

deriving stock instance (Show (QuantVariableAST a), Show (QuantVariableTarget a)) => Show (QuantVariableAssignment a)
deriving stock instance (Eq (QuantVariableAST a), Eq (QuantVariableTarget a)) => Eq (QuantVariableAssignment a)

type family QuantVariableTarget a where
  QuantVariableTarget 'H.Sym.Tok.IntQuantity = HexInt
  QuantVariableTarget 'H.Sym.Tok.LenQuantity = Length
  QuantVariableTarget 'H.Sym.Tok.GlueQuantity = Glue
  QuantVariableTarget 'H.Sym.Tok.MathGlueQuantity = MathGlue
  QuantVariableTarget 'H.Sym.Tok.TokenListQuantity = TokenListAssignmentTarget

data VariableAssignment
  = IntVariableAssignment (QuantVariableAssignment 'H.Sym.Tok.IntQuantity)
  | LengthVariableAssignment (QuantVariableAssignment 'H.Sym.Tok.LenQuantity)
  | GlueVariableAssignment (QuantVariableAssignment 'H.Sym.Tok.GlueQuantity)
  | MathGlueVariableAssignment (QuantVariableAssignment 'H.Sym.Tok.MathGlueQuantity)
  | TokenListVariableAssignment (QuantVariableAssignment 'H.Sym.Tok.TokenListQuantity)
  | SpecialIntParameterVariableAssignment H.Sym.Tok.SpecialIntParameter HexInt
  | SpecialLengthParameterVariableAssignment H.Sym.Tok.SpecialLengthParameter Length
  deriving stock (Show, Eq, Generic)

data VariableModification
  = AdvanceIntVariable (QuantVariableAST 'H.Sym.Tok.IntQuantity) (QuantVariableTarget 'H.Sym.Tok.IntQuantity)
  | AdvanceLengthVariable (QuantVariableAST 'H.Sym.Tok.LenQuantity) (QuantVariableTarget 'H.Sym.Tok.LenQuantity)
  | AdvanceGlueVariable (QuantVariableAST 'H.Sym.Tok.GlueQuantity) (QuantVariableTarget 'H.Sym.Tok.GlueQuantity)
  | AdvanceMathGlueVariable (QuantVariableAST 'H.Sym.Tok.MathGlueQuantity) (QuantVariableTarget 'H.Sym.Tok.MathGlueQuantity)
  | ScaleVariable H.Q.VDirection NumericVariable HexInt
  deriving stock (Show, Eq, Generic)

data NumericVariable
  = IntNumericVariable (QuantVariableAST 'H.Sym.Tok.IntQuantity)
  | LengthNumericVariable (QuantVariableAST 'H.Sym.Tok.LenQuantity)
  | GlueNumericVariable (QuantVariableAST 'H.Sym.Tok.GlueQuantity)
  | MathGlueNumericVariable (QuantVariableAST 'H.Sym.Tok.MathGlueQuantity)
  deriving stock (Show, Eq, Generic)

data CodeAssignment = CodeAssignment CodeTableRef HexInt
  deriving stock (Show, Eq, Generic)

data FontSpecification = NaturalFont | FontAt Length | FontScaled HexInt
  deriving stock (Show, Eq, Generic)

-- Box specification.
data Box
  = FetchedRegisterBox H.Sym.Tok.BoxFetchMode HexInt
  | LastBox
  | VSplitBox HexInt Length
  | ExplicitBox BoxSpecification H.Sym.Tok.ExplicitBox
  deriving stock (Show, Eq, Generic)

data BoxSpecification = Natural | To Length | Spread Length
  deriving stock (Show, Eq, Generic)

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule H.Q.Axis Rule
  deriving stock (Show, Eq, Generic)

data DiscretionaryText = DiscretionaryText {preBreak, postBreak, noBreak :: H.Sym.Tok.ExpandedBalancedText}
  deriving stock (Show, Eq, Generic)

data FetchedBoxRef = FetchedBoxRef HexInt H.Sym.Tok.BoxFetchMode
  deriving stock (Show, Eq, Generic)

data LeadersSpec = LeadersSpec H.Sym.Tok.LeadersType BoxOrRule Glue
  deriving stock (Show, Eq, Generic)

data CommandTrigger = CharCommandTrigger | CSCommandTrigger
  deriving stock (Show, Eq, Generic)

data InternalQuantity
  = InternalIntQuantity InternalInt
  | InternalLengthQuantity InternalLength
  | InternalGlueQuantity InternalGlue
  | InternalMathGlueQuantity InternalMathGlue
  | FontQuantity FontRef
  | TokenListVariableQuantity (QuantVariableAST 'H.Sym.Tok.TokenListQuantity)
  deriving stock (Show, Eq, Generic)

data WriteText
  = ImmediateWriteText H.Sym.Tok.ExpandedBalancedText
  | DeferredWriteText H.Sym.Tok.InhibitedBalancedText
  deriving stock (Show, Eq, Generic)

data WritePolicy = Immediate | Deferred
  deriving stock (Show, Eq, Generic)

newtype Rule = Rule (Seq (H.Q.BoxDim, Length))
  deriving stock (Show, Eq, Generic)

data FileStreamAction = Open HexFilePath | Close
  deriving stock (Show, Eq, Generic)

data FileStreamType = FileInput | FileOutput WritePolicy
  deriving stock (Show, Eq, Generic)

data BoxPlacement = NaturalPlacement | ShiftedPlacement H.Q.Axis H.Q.Direction Length
  deriving stock (Show, Eq, Generic)

data CharCodeRef
  = CharRef H.Code.CharCode
  | CharTokenRef H.Q.HexInt
  | CharCodeNrRef HexInt
  deriving stock (Show, Eq, Generic)
