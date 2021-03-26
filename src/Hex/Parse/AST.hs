module Hex.Parse.AST where

import Hex.Codes qualified as H.Code
import Hex.Lex.Types qualified as H.Lex
import Hex.Quantity qualified as H.Q
import Hex.Quantity qualified as H.Quant
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hexlude

-- HexInt.

type HexInt = H.Sym.Tok.Signed UnsignedHexInt

newtype EightBitHexInt = EightBitHexInt HexInt
  deriving stock (Show, Generic)

constHexInt :: H.Q.HexInt -> HexInt
constHexInt n = H.Sym.Tok.Signed H.Sym.Tok.Positive $ constUHexInt n

data UnsignedHexInt
  = NormalHexIntAsUHexInt NormalHexInt
  | CoercedHexInt CoercedHexInt
  deriving stock (Show, Generic)

constUHexInt :: H.Q.HexInt -> UnsignedHexInt
constUHexInt n = NormalHexIntAsUHexInt $ HexIntConstant n

-- Think: 'un-coerced integer'.
data NormalHexInt = HexIntConstant H.Q.HexInt | InternalHexInt InternalHexInt
  deriving stock (Show, Generic)

zeroInt :: NormalHexInt
zeroInt = HexIntConstant $ H.Q.HexInt 0

oneHexInt :: NormalHexInt
oneHexInt = HexIntConstant $ H.Q.HexInt 1

data CoercedHexInt
  = InternalLengthAsInt InternalLength
  | InternalGlueAsInt InternalGlue
  deriving stock (Show, Generic)

-- Length.
type Length = H.Sym.Tok.Signed UnsignedLength

zeroLength :: Length
zeroLength =
  H.Sym.Tok.Signed H.Sym.Tok.Positive $
    NormalLengthAsULength $
      LengthSemiConstant zeroFactor scaledPointUnit

data UnsignedLength
  = NormalLengthAsULength NormalLength
  | CoercedLength CoercedLength
  deriving stock (Show, Generic)

-- Think: 'un-coerced length'.
data NormalLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    LengthSemiConstant Factor Unit
  | InternalLength InternalLength
  deriving stock (Show, Generic)

data Factor
  = NormalHexIntFactor NormalHexInt
  | -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
    -- with decimal digits, but its main feature is that it can represent
    -- non-integers.
    RationalConstant Rational
  deriving stock (Show, Generic)

zeroFactor, oneFactor :: Factor
zeroFactor = NormalHexIntFactor zeroInt
oneFactor = NormalHexIntFactor oneHexInt

data Unit
  = PhysicalUnit PhysicalUnitFrame H.Quant.PhysicalUnit
  | InternalUnit InternalUnit
  deriving stock (Show, Generic)

scaledPointUnit :: Unit
scaledPointUnit = PhysicalUnit MagnifiedFrame H.Quant.ScaledPoint

data InternalUnit
  = Em
  | Ex
  | InternalHexIntUnit InternalHexInt
  | InternalLengthUnit InternalLength
  | InternalGlueUnit InternalGlue
  deriving stock (Show, Generic)

data PhysicalUnitFrame = MagnifiedFrame | TrueFrame
  deriving stock (Show, Generic)

newtype CoercedLength = InternalGlueAsLength InternalGlue
  deriving stock (Show, Generic)

-- Math-length.
type MathLength = H.Sym.Tok.Signed UnsignedMathLength

data UnsignedMathLength
  = NormalMathLengthAsUMathLength NormalMathLength
  | CoercedMathLength CoercedMathLength
  deriving stock (Show, Generic)

-- Think: 'un-coerced length'.
data NormalMathLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    MathLengthSemiConstant Factor MathUnit
  deriving stock (Show, Generic)

data MathUnit = Mu | InternalMathGlueAsUnit InternalMathGlue
  deriving stock (Show, Generic)

newtype CoercedMathLength = InternalMathGlueAsMathLength InternalMathGlue
  deriving stock (Show, Generic)

-- Glue.
data Glue
  = ExplicitGlue Length (Maybe Flex) (Maybe Flex)
  | InternalGlue (H.Sym.Tok.Signed InternalGlue)
  deriving stock (Show, Generic)

data Flex = FiniteFlex Length | FilFlex FilLength
  deriving stock (Show, Generic)

oneFilFlex, minusOneFilFlex, oneFillFlex :: Flex
oneFilFlex = FilFlex oneFil
minusOneFilFlex = FilFlex minusOneFil
oneFillFlex = FilFlex oneFill

data FilLength = FilLength (H.Sym.Tok.Signed Factor) Int
  deriving stock (Show, Generic)

oneFil, minusOneFil, oneFill :: FilLength
oneFil = FilLength (H.Sym.Tok.Signed H.Sym.Tok.Positive oneFactor) 1
minusOneFil = FilLength (H.Sym.Tok.Signed H.Sym.Tok.Negative oneFactor) 1
oneFill = FilLength (H.Sym.Tok.Signed H.Sym.Tok.Positive oneFactor) 2

-- Math glue.
data MathGlue
  = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
  | InternalMathGlue H.Sym.Tok.Sign InternalMathGlue
  deriving stock (Show, Generic)

data MathFlex = FiniteMathFlex MathLength | FilMathFlex FilLength
  deriving stock (Show, Generic)

-- Internal quantities.
data QuantVariable a = ParamVar a | RegisterVar EightBitHexInt
  deriving stock (Show, Generic)

type HexIntVariable = QuantVariable H.Sym.Tok.IntParameter

type LengthVariable = QuantVariable H.Sym.Tok.LengthParameter

type GlueVariable = QuantVariable H.Sym.Tok.GlueParameter

type MathGlueVariable = QuantVariable H.Sym.Tok.MathGlueParameter

type TokenListVariable = QuantVariable H.Sym.Tok.TokenListParameter

data InternalHexInt
  = InternalHexIntVariable HexIntVariable
  | InternalSpecialIntParameter H.Sym.Tok.SpecialIntParameter
  | InternalCodeTableRef CodeTableRef
  | InternalCharToken HexInt
  | InternalMathCharToken HexInt
  | InternalFontCharRef FontCharRef
  | LastPenalty
  | ParShape
  | InputLineNr
  | Badness
  deriving stock (Show, Generic)

data CodeTableRef = CodeTableRef H.Sym.Tok.CodeType HexInt
  deriving stock (Show, Generic)

data FontCharRef = FontCharRef H.Sym.Tok.FontChar FontRef
  deriving stock (Show, Generic)

data FontRef
  = FontTokenRef HexInt
  | CurrentFontRef
  | FamilyMemberFontRef FamilyMember
  deriving stock (Show, Generic)

data FamilyMember = FamilyMember H.Sym.Tok.FontRange HexInt
  deriving stock (Show, Generic)

data BoxDimensionRef = BoxDimensionRef EightBitHexInt H.Quant.BoxDim
  deriving stock (Show, Generic)

data FontDimensionRef = FontDimensionRef HexInt FontRef
  deriving stock (Show, Generic)

data InternalLength
  = InternalLengthVariable LengthVariable
  | InternalSpecialLengthParameter H.Sym.Tok.SpecialLengthParameter
  | InternalFontDimensionRef FontDimensionRef
  | InternalBoxDimensionRef BoxDimensionRef
  | LastKern
  deriving stock (Show, Generic)

data InternalGlue = InternalGlueVariable GlueVariable | LastGlue
  deriving stock (Show, Generic)

data InternalMathGlue
  = InternalMathGlueVariable MathGlueVariable
  | LastMathGlue
  deriving stock (Show, Generic)

-- Assignments.
data Assignment = Assignment {body :: AssignmentBody, scope :: H.Sym.Tok.ScopeFlag}
  deriving stock (Show, Generic)

newtype HexFilePath = HexFilePath FilePath
  deriving stock (Show, Generic)

data ControlSequenceTarget
  = MacroTarget H.Sym.Tok.MacroContents
  | LetTarget H.Lex.LexToken
  | FutureLetTarget H.Lex.LexToken H.Lex.LexToken
  | ShortDefineTarget H.Sym.Tok.QuantityType HexInt
  | ReadTarget HexInt
  | FontTarget FontSpecification HexFilePath
  deriving stock (Show, Generic)

data AssignmentBody
  = DefineControlSequence H.Lex.LexSymbol ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont HexInt
  | SetFamilyMember FamilyMember FontRef
  | SetParShape [(Length, Length)]
  | SetBoxRegister EightBitHexInt Box
  | -- -- Global assignments.
    SetFontDimension FontDimensionRef Length
  | SetFontChar FontCharRef HexInt
  | SetHyphenation H.Sym.Tok.BalancedText
  | SetHyphenationPatterns H.Sym.Tok.BalancedText
  | SetBoxDimension BoxDimensionRef Length
  | SetInteractionMode H.Sym.Tok.InteractionMode
  deriving stock (Show, Generic)

data TokenListAssignmentTarget
  = TokenListAssignmentVar TokenListVariable
  | TokenListAssignmentText H.Sym.Tok.BalancedText
  deriving stock (Show, Generic)

data VariableAssignment
  = HexIntVariableAssignment HexIntVariable HexInt
  | LengthVariableAssignment LengthVariable Length
  | GlueVariableAssignment GlueVariable Glue
  | MathGlueVariableAssignment MathGlueVariable MathGlue
  | TokenListVariableAssignment TokenListVariable TokenListAssignmentTarget
  | SpecialIntParameterVariableAssignment H.Sym.Tok.SpecialIntParameter HexInt
  | SpecialLengthParameterVariableAssignment H.Sym.Tok.SpecialLengthParameter Length
  deriving stock (Show, Generic)

data VariableModification
  = AdvanceHexIntVariable HexIntVariable HexInt
  | AdvanceLengthVariable LengthVariable Length
  | AdvanceGlueVariable GlueVariable Glue
  | AdvanceMathGlueVariable MathGlueVariable MathGlue
  | ScaleVariable H.Quant.VDirection NumericVariable HexInt
  deriving stock (Show, Generic)

data NumericVariable
  = HexIntNumericVariable HexIntVariable
  | LengthNumericVariable LengthVariable
  | GlueNumericVariable GlueVariable
  | MathGlueNumericVariable MathGlueVariable
  deriving stock (Show, Generic)

data CodeAssignment = CodeAssignment CodeTableRef HexInt
  deriving stock (Show, Generic)

data FontSpecification = NaturalFont | FontAt Length | FontScaled HexInt
  deriving stock (Show, Generic)

-- Box specification.
data Box
  = FetchedRegisterBox H.Sym.Tok.BoxFetchMode EightBitHexInt
  | LastBox
  | VSplitBox HexInt Length
  | ExplicitBox BoxSpecification H.Sym.Tok.ExplicitBox
  deriving stock (Show, Generic)

data BoxSpecification = Natural | To Length | Spread Length
  deriving stock (Show, Generic)

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule H.Quant.Axis Rule
  deriving stock (Show, Generic)

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
  | ChangeScope H.Sym.Tok.Sign CommandTrigger
  deriving stock (Show, Generic)

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
  deriving stock (Show, Generic)

data VModeCommand
  = End
  | Dump
  | EnterHMode
  | AddVGlue Glue
  | AddVLeaders LeadersSpec
  | AddVRule Rule
  | AddUnwrappedFetchedVBox FetchedBoxRef -- \unv{box,copy}
  deriving stock (Show, Generic)

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
  deriving stock (Show, Generic)

data DiscretionaryText = DiscretionaryText {preBreak, postBreak, noBreak :: H.Sym.Tok.BalancedText}
  deriving stock (Show, Generic)

data FetchedBoxRef = FetchedBoxRef HexInt H.Sym.Tok.BoxFetchMode
  deriving stock (Show, Generic)

data LeadersSpec = LeadersSpec H.Sym.Tok.LeadersType BoxOrRule Glue
  deriving stock (Show, Generic)

data CommandTrigger = CharCommandTrigger | CSCommandTrigger
  deriving stock (Show, Eq, Generic)

data InternalQuantity
  = InternalHexIntQuantity InternalHexInt
  | InternalLengthQuantity InternalLength
  | InternalGlueQuantity InternalGlue
  | InternalMathGlueQuantity InternalMathGlue
  | FontQuantity FontRef
  | TokenListVariableQuantity TokenListVariable
  deriving stock (Show, Generic)

data WriteText
  = ImmediateWriteText H.Sym.Tok.ExpandedBalancedText
  | DeferredWriteText H.Sym.Tok.BalancedText
  deriving stock (Show, Generic)

data WritePolicy = Immediate | Deferred
  deriving stock (Show, Generic)

data Rule = Rule {width, height, depth :: Maybe Length}
  deriving stock (Show, Generic)

data FileStreamAction = Open HexFilePath | Close
  deriving stock (Show, Generic)

data FileStreamType = FileInput | FileOutput WritePolicy
  deriving stock (Show, Generic)

data BoxPlacement = NaturalPlacement | ShiftedPlacement H.Quant.Axis H.Quant.Direction Length
  deriving stock (Show, Generic)

data CharCodeRef
  = CharRef H.Code.CharCode
  | CharTokenRef H.Q.HexInt
  | CharCodeNrRef HexInt
  deriving stock (Show, Generic)

-- Condition heads.
data IfConditionHead
  = IfHexIntPairTest HexInt Ordering HexInt -- \ifnum
  | IfLengthPairTest Length Ordering Length -- \ifdim
  | IfHexIntOdd HexInt -- \ifodd
  | IfInMode H.Sym.Tok.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual H.Sym.Tok.TokenAttribute H.Sym.Tok.PrimitiveToken H.Sym.Tok.PrimitiveToken -- \if, \ifcat
  | IfTokensEqual H.Lex.LexToken H.Lex.LexToken -- \ifx
  | IfBoxRegisterIs H.Sym.Tok.BoxRegisterAttribute HexInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded HexInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Show, Generic)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead HexInt
  deriving stock (Show, Generic)
