module Hex.Stage.Evaluate.Interface.AST.Command where

import Hex.Common.HexState.Interface.Resolve qualified as Res
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as Res.PT
import Hex.Common.HexState.Interface.Resolve.SyntaxToken qualified as Res.ST
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Interpret.Build.Box.Elem (FontSpecification, HexFilePath, Kern, Rule)
import Hex.Stage.Interpret.Build.List.Elem (Penalty)
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Interface.AST.Command qualified as Uneval
import Hex.Stage.Parse.Interface.AST.Common qualified as Uneval
import Hexlude
import qualified Hex.Common.Codes as Code
import qualified ASCII

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
  | AddMark Res.ST.ExpandedBalancedText
  | -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- then can 'migrate' out.
    AddInsertion Q.HexInt VModeMaterial
  | AddAdjustment VModeMaterial
  | AddSpace
  | StartParagraph Res.PT.IndentFlag
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

data Assignment = Assignment {body :: AssignmentBody, scope :: Res.PT.ScopeFlag}
  deriving stock (Show, Eq, Generic)

data ModeIndependentCommand
  = Assign Assignment
  | Relax
  | IgnoreSpaces
  | AddPenalty Penalty
  | AddKern Kern
  | AddMathKern Uneval.MathLength
  | RemoveItem Res.PT.RemovableItem
  | SetAfterAssignmentToken Lex.LexToken
  | AddToAfterGroupTokens Lex.LexToken
  | WriteMessage MessageWriteCommand
  | ModifyFileStream Uneval.FileStreamModificationCommand
  | WriteToStream Uneval.StreamWriteCommand
  | DoSpecial Res.ST.ExpandedBalancedText
  | AddBox Uneval.BoxPlacement Uneval.Box
  | ChangeScope Q.Sign Uneval.CommandTrigger
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
  | SetVariable Uneval.VariableAssignment
  | ModifyVariable Uneval.VariableModification
  | AssignCode CodeAssignment
  | SelectFont Res.PT.FontNumber
  | SetFamilyMember Uneval.FamilyMember Uneval.FontRef
  | SetParShape Uneval.HexInt [Uneval.Length]
  | SetBoxRegister Uneval.HexInt Uneval.Box
  | -- Global assignments.
    SetFontDimension Uneval.FontDimensionRef Uneval.Length
  | SetFontChar Uneval.FontCharRef Uneval.HexInt
  | SetHyphenation Res.ST.InhibitedBalancedText
  | SetHyphenationPatterns Res.ST.InhibitedBalancedText
  | SetBoxDimension Uneval.BoxDimensionRef Uneval.Length
  | SetInteractionMode Res.PT.InteractionMode
  deriving stock (Show, Eq, Generic)

data CodeAssignment = CodeAssignment {codeIndex :: Code.CharCode, codeValue :: CodeValue}
  deriving stock (Show, Eq, Generic)

data CodeValue
  = CatCodeValue Code.CatCode
  | MathCodeValue Code.MathCode
  | ChangeCaseCodeValue ASCII.Case Code.CaseChangeCode
  | SpaceFactorCodeValue Code.SpaceFactorCode
  | DelimiterCodeValue Code.DelimiterCode
  deriving stock (Show, Eq, Generic)

data MessageWriteCommand = MessageWriteCommand Res.PT.StandardOutputSource Text
  deriving stock (Show, Eq, Generic)

data ControlSequenceTarget
  = NonFontTarget Res.ResolvedToken
  | FontTarget FontFileSpec
  deriving stock (Show, Eq, Generic)

data FontFileSpec = FontFileSpec FontSpecification HexFilePath
  deriving stock (Show, Eq, Generic)
