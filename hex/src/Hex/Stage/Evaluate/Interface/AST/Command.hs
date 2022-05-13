{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Evaluate.Interface.AST.Command where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as ST
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Evaluate.Interface.AST.Quantity qualified as Eval
import Hex.Stage.Interpret.Build.Box.Elem (FontSpecification, Kern, Rule)
import Hex.Stage.Interpret.Build.List.Elem (Penalty)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hex.Stage.Parse.Interface.AST.Quantity qualified as Uneval
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
  = ShowToken Lex.LexToken
  | ShowBox Q.HexInt
  | ShowLists
  | ShowTheInternalQuantity Uneval.InternalQuantity
  | ShipOut Uneval.Box
  | AddMark ST.ExpandedBalancedText
  | -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- then can 'migrate' out.
    AddInsertion Q.HexInt VModeMaterial
  | AddAdjustment VModeMaterial
  | AddSpace
  | StartParagraph PT.IndentFlag
  | EndParagraph
  | AddAlignedMaterial
      DesiredLength
      AlignmentMaterial
      HModeCommand
      HModeCommand
  | HModeCommand HModeCommand
  | VModeCommand Uneval.VModeCommand
  | ModeIndependentCommand ModeIndependentCommand
  deriving stock (Show, Eq, Generic)

-- Not implemented.
data VModeMaterial
  deriving stock (Show, Eq, Generic)

-- Not implemented.
data AlignmentMaterial
  deriving stock (Show, Eq, Generic)

-- Not implemented.
data DesiredLength
  deriving stock (Show, Eq, Generic)

data Assignment = Assignment {body :: AssignmentBody, scope :: PT.ScopeFlag}
  deriving stock (Show, Eq, Generic)

data ModeIndependentCommand
  = Assign Assignment
  | Relax
  | IgnoreSpaces
  | AddPenalty Penalty
  | AddKern Kern
  | AddMathKern Uneval.MathLength
  | RemoveItem PT.RemovableItem
  | SetAfterAssignmentToken Lex.LexToken
  | AddToAfterGroupTokens Lex.LexToken
  | WriteMessage MessageWriteCommand
  | ModifyFileStream Uneval.FileStreamModificationCommand
  | WriteToStream Uneval.StreamWriteCommand
  | DoSpecial ST.ExpandedBalancedText
  | AddBox Uneval.BoxPlacement Uneval.Box
  | ChangeScope Q.Sign Uneval.CommandTrigger
  | DebugShowState
  deriving stock (Show, Eq, Generic)

data HModeCommand
  = AddControlSpace
  | AddCharacter Uneval.CharCodeRef
  | AddAccentedCharacter Uneval.HexInt [Assignment] (Maybe Uneval.CharCodeRef)
  | AddItalicCorrection
  | AddDiscretionaryText Uneval.DiscretionaryText
  | AddDiscretionaryHyphen
  | EnterMathMode
  | AddHGlue Q.Glue
  | AddHLeaders Uneval.LeadersSpec
  | AddHRule Rule
  | AddUnwrappedFetchedHBox Uneval.FetchedBoxRef -- \unh{box,copy}
  deriving stock (Show, Eq, Generic)

data AssignmentBody
  = DefineControlSequence Res.ControlSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont PT.FontNumber
  | SetFamilyMember Uneval.FamilyMember Uneval.FontRef
  | SetParShape Uneval.HexInt [Uneval.Length]
  | SetBoxRegister Uneval.HexInt Uneval.Box
  | -- Global assignments.
    SetFontDimension Uneval.FontDimensionRef Uneval.Length
  | SetFontChar Uneval.FontCharRef Uneval.HexInt
  | SetHyphenation ST.InhibitedBalancedText
  | SetHyphenationPatterns ST.InhibitedBalancedText
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

data MessageWriteCommand = MessageWriteCommand {messageDest :: PT.StandardOutputSource, messageContents :: Text}
  deriving stock (Show, Eq, Generic)

data ControlSequenceTarget
  = MacroTarget ST.MacroDefinition
  | LetTarget Lex.LexToken
  | FutureLetTarget Uneval.FutureLetDefinition
  | ShortDefineTarget PT.CharryQuantityType Q.HexInt
  | ReadTarget Q.HexInt
  | FontTarget FontFileSpec
  deriving stock (Show, Eq, Generic)

data FontFileSpec = FontFileSpec FontSpecification Q.HexFilePath
  deriving stock (Show, Eq, Generic)

data VariableAssignment
  = IntVariableAssignment (QuantVariableAssignment 'PT.IntQuantity)
  | LengthVariableAssignment (QuantVariableAssignment 'PT.LengthQuantity)
  | GlueVariableAssignment (QuantVariableAssignment 'PT.GlueQuantity)
  | MathGlueVariableAssignment (QuantVariableAssignment 'PT.MathGlueQuantity)
  | TokenListVariableAssignment (QuantVariableAssignment 'PT.TokenListQuantity)
  | SpecialIntParameterVariableAssignment PT.SpecialIntParameter Q.HexInt
  | SpecialLengthParameterVariableAssignment PT.SpecialLengthParameter Q.Length
  deriving stock (Show, Eq, Generic)

data QuantVariableAssignment (q :: PT.QuantityType) = QuantVariableAssignment (Eval.QuantVariableEval q) (Eval.QuantVariableTargetEval q)
  deriving stock (Generic)

deriving stock instance (Show (Eval.QuantVariableEval a), Show (Eval.QuantVariableTargetEval a)) => Show (QuantVariableAssignment a)

deriving stock instance (Eq (Eval.QuantVariableEval a), Eq (Eval.QuantVariableTargetEval a)) => Eq (QuantVariableAssignment a)

data VariableModification
  = AdvanceIntVariable (Eval.QuantVariableEval 'PT.IntQuantity) (Eval.QuantVariableTargetEval 'PT.IntQuantity)
  | AdvanceLengthVariable (Eval.QuantVariableEval 'PT.LengthQuantity) (Eval.QuantVariableTargetEval 'PT.LengthQuantity)
  | AdvanceGlueVariable (Eval.QuantVariableEval 'PT.GlueQuantity) (Eval.QuantVariableTargetEval 'PT.GlueQuantity)
  | AdvanceMathGlueVariable (Eval.QuantVariableEval 'PT.MathGlueQuantity) (Eval.QuantVariableTargetEval 'PT.MathGlueQuantity)
  | ScaleVariable Q.VDirection NumericVariable Q.HexInt
  deriving stock (Show, Eq, Generic)

data NumericVariable
  = IntNumericVariable (Eval.QuantVariableEval 'PT.IntQuantity)
  | LengthNumericVariable (Eval.QuantVariableEval 'PT.LengthQuantity)
  | GlueNumericVariable (Eval.QuantVariableEval 'PT.GlueQuantity)
  | MathGlueNumericVariable (Eval.QuantVariableEval 'PT.MathGlueQuantity)
  deriving stock (Show, Eq, Generic)
