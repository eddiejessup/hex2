module Hex.Parse.AST where

import Hex.Codes qualified as H.Code
import Hex.Lex.Types qualified as H.Lex
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hexlude

data Signed a = Signed [H.Q.Sign] a
  deriving stock (Show, Eq, Generic)

-- HexInt.

newtype HexInt = HexInt {unInt :: Signed UnsignedInt}
  deriving stock (Show, Eq, Generic)

data UnsignedInt
  = NormalUnsignedInt NormalInt
  | CoercedUnsignedInt CoercedInt
  deriving stock (Show, Eq, Generic)

data IntBase = Base8 | Base10 | Base16
  deriving stock (Show, Eq, Generic)

data IntConstantDigits = IntConstantDigits IntBase [Word8]
  deriving stock (Show, Eq, Generic)

-- Think: 'un-coerced integer'.
data NormalInt = IntConstant IntConstantDigits | CharLikeCode Word8 | InternalInt InternalInt
  deriving stock (Show, Eq, Generic)

zeroInt :: NormalInt
zeroInt = IntConstant $ IntConstantDigits Base10 [0]

oneInt :: NormalInt
oneInt = IntConstant $ IntConstantDigits Base10 [1]

data CoercedInt
  = InternalLengthAsInt InternalLength
  | InternalGlueAsInt InternalGlue
  deriving stock (Show, Eq, Generic)

-- Length.
type Length = Signed UnsignedLength

zeroLength :: Length
zeroLength =
  Signed [H.Q.Positive] $
    NormalLengthAsULength $
      LengthSemiConstant zeroFactor scaledPointUnit

data UnsignedLength
  = NormalLengthAsULength NormalLength
  | CoercedLength CoercedLength
  deriving stock (Show, Eq, Generic)

-- Think: 'un-coerced length'.
data NormalLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    LengthSemiConstant Factor Unit
  | InternalLength InternalLength
  deriving stock (Show, Eq, Generic)

data Factor
  = NormalIntFactor NormalInt
  | -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
    -- with decimal digits, but its main feature is that it can represent
    -- non-integers.
    RationalConstant Rational
  deriving stock (Show, Eq, Generic)

zeroFactor, oneFactor :: Factor
zeroFactor = NormalIntFactor zeroInt
oneFactor = NormalIntFactor oneInt

data Unit
  = PhysicalUnit PhysicalUnitFrame H.Q.PhysicalUnit
  | InternalUnit InternalUnit
  deriving stock (Show, Eq, Generic)

scaledPointUnit :: Unit
scaledPointUnit = PhysicalUnit MagnifiedFrame H.Q.ScaledPoint

data InternalUnit
  = Em
  | Ex
  | InternalIntUnit InternalInt
  | InternalLengthUnit InternalLength
  | InternalGlueUnit InternalGlue
  deriving stock (Show, Eq, Generic)

data PhysicalUnitFrame = MagnifiedFrame | TrueFrame
  deriving stock (Show, Eq, Generic)

newtype CoercedLength = InternalGlueAsLength InternalGlue
  deriving stock (Show, Eq, Generic)

-- Math-length.
type MathLength = Signed UnsignedMathLength

data UnsignedMathLength
  = NormalMathLengthAsUMathLength NormalMathLength
  | CoercedMathLength CoercedMathLength
  deriving stock (Show, Eq, Generic)

-- Think: 'un-coerced length'.
data NormalMathLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    MathLengthSemiConstant Factor MathUnit
  deriving stock (Show, Eq, Generic)

data MathUnit = Mu | InternalMathGlueAsUnit InternalMathGlue
  deriving stock (Show, Eq, Generic)

newtype CoercedMathLength = InternalMathGlueAsMathLength InternalMathGlue
  deriving stock (Show, Eq, Generic)

-- Glue.
data Glue
  = ExplicitGlue Length (Maybe Flex) (Maybe Flex)
  | InternalGlue (Signed InternalGlue)
  deriving stock (Show, Eq, Generic)

data Flex = FiniteFlex Length | FilFlex FilLength
  deriving stock (Show, Eq, Generic)

oneFilFlex, minusOneFilFlex, oneFillFlex :: Flex
oneFilFlex = FilFlex oneFil
minusOneFilFlex = FilFlex minusOneFil
oneFillFlex = FilFlex oneFill

data FilLength = FilLength (Signed Factor) H.Q.InfLengthOrder
  deriving stock (Show, Eq, Generic)

oneFil, minusOneFil, oneFill :: FilLength
oneFil = FilLength (Signed [H.Q.Positive] oneFactor) H.Q.Fil1
minusOneFil = FilLength (Signed [H.Q.Negative] oneFactor) H.Q.Fil1
oneFill = FilLength (Signed [H.Q.Positive] oneFactor) H.Q.Fil2

-- Math glue.
data MathGlue
  = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
  | InternalMathGlue (Signed InternalMathGlue)
  deriving stock (Show, Eq, Generic)

data MathFlex = FiniteMathFlex MathLength | FilMathFlex FilLength
  deriving stock (Show, Eq, Generic)

-- Internal quantities.
data QuantVariable a = ParamVar a | RegisterVar RegisterLocation
  deriving stock (Show, Eq, Generic)

data RegisterLocation = ExplicitRegisterLocation HexInt | InternalRegisterLocation H.Q.HexInt
  deriving stock (Show, Eq, Generic)

type IntVariable = QuantVariable H.Sym.Tok.IntParameter

type LengthVariable = QuantVariable H.Sym.Tok.LengthParameter

type GlueVariable = QuantVariable H.Sym.Tok.GlueParameter

type MathGlueVariable = QuantVariable H.Sym.Tok.MathGlueParameter

type TokenListVariable = QuantVariable H.Sym.Tok.TokenListParameter

data InternalInt
  = InternalIntVariable IntVariable
  | InternalSpecialIntParameter H.Sym.Tok.SpecialIntParameter
  | InternalCodeTableRef CodeTableRef
  | InternalCharToken H.Q.HexInt
  | InternalMathCharToken H.Q.HexInt
  | InternalFontCharRef FontCharRef
  | LastPenalty
  | ParShape
  | InputLineNr
  | Badness
  deriving stock (Show, Eq, Generic)

data CodeTableRef = CodeTableRef H.Sym.Tok.CodeType HexInt
  deriving stock (Show, Eq, Generic)

data FontCharRef = FontCharRef H.Sym.Tok.FontChar FontRef
  deriving stock (Show, Eq, Generic)

data FontRef
  = FontTokenRef H.Q.HexInt
  | CurrentFontRef
  | FamilyMemberFontRef FamilyMember
  deriving stock (Show, Eq, Generic)

data FamilyMember = FamilyMember H.Sym.Tok.FontRange HexInt
  deriving stock (Show, Eq, Generic)

data BoxDimensionRef = BoxDimensionRef HexInt H.Q.BoxDim
  deriving stock (Show, Eq, Generic)

data FontDimensionRef = FontDimensionRef HexInt FontRef
  deriving stock (Show, Eq, Generic)

data InternalLength
  = InternalLengthVariable LengthVariable
  | InternalSpecialLengthParameter H.Sym.Tok.SpecialLengthParameter
  | InternalFontDimensionRef FontDimensionRef
  | InternalBoxDimensionRef BoxDimensionRef
  | LastKern
  deriving stock (Show, Eq, Generic)

data InternalGlue = InternalGlueVariable GlueVariable | LastGlue
  deriving stock (Show, Eq, Generic)

data InternalMathGlue
  = InternalMathGlueVariable MathGlueVariable
  | LastMathGlue
  deriving stock (Show, Eq, Generic)

-- Assignments.
data Assignment = Assignment {body :: AssignmentBody, scope :: H.Sym.Tok.ScopeFlag}
  deriving stock (Show, Eq, Generic)

newtype HexFilePath = HexFilePath FilePath
  deriving stock (Show, Eq, Generic)

data ControlSequenceTarget
  = MacroTarget H.Sym.Tok.MacroContents
  | LetTarget H.Lex.LexToken
  | FutureLetTarget H.Lex.LexToken H.Lex.LexToken
  | ShortDefineTarget H.Sym.Tok.QuantityType HexInt
  | ReadTarget HexInt
  | FontTarget FontSpecification HexFilePath
  deriving stock (Show, Eq, Generic)

data AssignmentBody
  = DefineControlSequence H.Lex.LexSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont HexInt
  | SetFamilyMember FamilyMember FontRef
  | SetParShape [(Length, Length)]
  | SetBoxRegister HexInt Box
  | -- -- Global assignments.
    SetFontDimension FontDimensionRef Length
  | SetFontChar FontCharRef HexInt
  | SetHyphenation H.Sym.Tok.BalancedText
  | SetHyphenationPatterns H.Sym.Tok.BalancedText
  | SetBoxDimension BoxDimensionRef Length
  | SetInteractionMode H.Sym.Tok.InteractionMode
  deriving stock (Show, Eq, Generic)

data TokenListAssignmentTarget
  = TokenListAssignmentVar TokenListVariable
  | TokenListAssignmentText H.Sym.Tok.BalancedText
  deriving stock (Show, Eq, Generic)

data VariableAssignment
  = IntVariableAssignment IntVariable HexInt
  | LengthVariableAssignment LengthVariable Length
  | GlueVariableAssignment GlueVariable Glue
  | MathGlueVariableAssignment MathGlueVariable MathGlue
  | TokenListVariableAssignment TokenListVariable TokenListAssignmentTarget
  | SpecialIntParameterVariableAssignment H.Sym.Tok.SpecialIntParameter HexInt
  | SpecialLengthParameterVariableAssignment H.Sym.Tok.SpecialLengthParameter Length
  deriving stock (Show, Eq, Generic)

data VariableModification
  = AdvanceIntVariable IntVariable HexInt
  | AdvanceLengthVariable LengthVariable Length
  | AdvanceGlueVariable GlueVariable Glue
  | AdvanceMathGlueVariable MathGlueVariable MathGlue
  | ScaleVariable H.Q.VDirection NumericVariable HexInt
  deriving stock (Show, Eq, Generic)

data NumericVariable
  = IntNumericVariable IntVariable
  | LengthNumericVariable LengthVariable
  | GlueNumericVariable GlueVariable
  | MathGlueNumericVariable MathGlueVariable
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

-- Commands.

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
  | Message H.Sym.Tok.StandardOutputStream H.Sym.Tok.ExpandedBalancedText
  | ModifyFileStream FileStreamType FileStreamAction HexInt
  | WriteToStream HexInt WriteText
  | DoSpecial H.Sym.Tok.ExpandedBalancedText
  | AddBox BoxPlacement Box
  | ChangeScope H.Q.Sign CommandTrigger
  deriving stock (Show, Eq, Generic)

data Command
  = ShowToken H.Lex.LexToken
  | ShowBox HexInt
  | ShowLists
  | ShowTheInternalQuantity InternalQuantity
  | ShipOut Box
  | AddMark H.Sym.Tok.BalancedText
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

data DiscretionaryText = DiscretionaryText {preBreak, postBreak, noBreak :: H.Sym.Tok.BalancedText}
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
  | TokenListVariableQuantity TokenListVariable
  deriving stock (Show, Eq, Generic)

data WriteText
  = ImmediateWriteText H.Sym.Tok.ExpandedBalancedText
  | DeferredWriteText H.Sym.Tok.BalancedText
  deriving stock (Show, Eq, Generic)

data WritePolicy = Immediate | Deferred
  deriving stock (Show, Eq, Generic)

data Rule = Rule {width, height, depth :: Maybe Length}
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

-- Condition heads.
data IfConditionHead
  = IfIntPairTest HexInt Ordering HexInt -- \ifnum
  | IfLengthPairTest Length Ordering Length -- \ifdim
  | IfIntOdd HexInt -- \ifodd
  | IfInMode H.Sym.Tok.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual H.Sym.Tok.TokenAttribute H.Sym.Tok.PrimitiveToken H.Sym.Tok.PrimitiveToken -- \if, \ifcat
  | IfTokensEqual H.Lex.LexToken H.Lex.LexToken -- \ifx
  | IfBoxRegisterIs H.Sym.Tok.BoxRegisterAttribute HexInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded HexInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Show, Eq, Generic)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead HexInt
  deriving stock (Show, Eq, Generic)
