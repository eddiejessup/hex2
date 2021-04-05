{-# LANGUAGE UndecidableInstances #-}
module Hex.Parse.AST where

import Hex.Codes qualified as H.Code
import Hex.Lex.Types qualified as H.Lex
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hexlude
import qualified Hex.Symbol.Types as H.Sym

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
    DecimalFractionFactor DecimalFraction
  deriving stock (Show, Eq, Generic)

zeroFactor, oneFactor :: Factor
zeroFactor = NormalIntFactor zeroInt
oneFactor = NormalIntFactor oneInt

data DecimalFraction = DecimalFraction {wholeDigits :: [Word8], fracDigits :: [Word8]}
  deriving stock (Show, Eq, Generic)

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
  = ExplicitGlue ExplicitGlueSpec
  | InternalGlue (Signed InternalGlue)
  deriving stock (Show, Eq, Generic)

data ExplicitGlueSpec = ExplicitGlueSpec {egLength :: Length, egStretch :: Maybe Flex, egShrink :: Maybe Flex}
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

type family QuantParam (a :: H.Sym.Tok.QuantityType) where
  QuantParam 'H.Sym.Tok.IntQuantity = H.Sym.Tok.IntParameter
  QuantParam 'H.Sym.Tok.LenQuantity = H.Sym.Tok.LengthParameter
  QuantParam 'H.Sym.Tok.GlueQuantity = H.Sym.Tok.GlueParameter
  QuantParam 'H.Sym.Tok.MathGlueQuantity = H.Sym.Tok.MathGlueParameter
  QuantParam 'H.Sym.Tok.TokenListQuantity = H.Sym.Tok.TokenListParameter

type family QuantVariableTargetAST a where
  QuantVariableTargetAST 'H.Sym.Tok.IntQuantity = HexInt
  QuantVariableTargetAST 'H.Sym.Tok.LenQuantity = Length
  QuantVariableTargetAST 'H.Sym.Tok.GlueQuantity = Glue
  QuantVariableTargetAST 'H.Sym.Tok.MathGlueQuantity = MathGlue
  QuantVariableTargetAST 'H.Sym.Tok.TokenListQuantity = TokenListAssignmentTarget

-- Internal quantities.
data QuantVariableAST (a :: H.Sym.Tok.QuantityType) = ParamVar (QuantParam a) | RegisterVar RegisterLocation
  deriving stock (Generic)

deriving stock instance Show (QuantParam a) => Show (QuantVariableAST a)
deriving stock instance Eq (QuantParam a) => Eq (QuantVariableAST a)

data RegisterLocation = ExplicitRegisterLocation HexInt | InternalRegisterLocation H.Q.HexInt
  deriving stock (Show, Eq, Generic)

data InternalInt
  = InternalIntVariable (QuantVariableAST 'H.Sym.Tok.IntQuantity)
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
  = InternalLengthVariable (QuantVariableAST 'H.Sym.Tok.LenQuantity)
  | InternalSpecialLengthParameter H.Sym.Tok.SpecialLengthParameter
  | InternalFontDimensionRef FontDimensionRef
  | InternalBoxDimensionRef BoxDimensionRef
  | LastKern
  deriving stock (Show, Eq, Generic)

data InternalGlue = InternalGlueVariable (QuantVariableAST 'H.Sym.Tok.GlueQuantity) | LastGlue
  deriving stock (Show, Eq, Generic)

data InternalMathGlue
  = InternalMathGlueVariable (QuantVariableAST 'H.Sym.Tok.MathGlueQuantity)
  | LastMathGlue
  deriving stock (Show, Eq, Generic)

-- Assignments.
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

data QuantVariableAssignment (q :: H.Sym.Tok.QuantityType) = QuantVariableAssignment (QuantVariableAST q) (QuantVariableTargetAST q)
  deriving stock (Generic)

deriving stock instance (Show (QuantVariableAST a), Show (QuantVariableTargetAST a)) => Show (QuantVariableAssignment a)
deriving stock instance (Eq (QuantVariableAST a), Eq (QuantVariableTargetAST a)) => Eq (QuantVariableAssignment a)

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
  = AdvanceIntVariable (QuantVariableAST 'H.Sym.Tok.IntQuantity) (QuantVariableTargetAST 'H.Sym.Tok.IntQuantity)
  | AdvanceLengthVariable (QuantVariableAST 'H.Sym.Tok.LenQuantity) (QuantVariableTargetAST 'H.Sym.Tok.LenQuantity)
  | AdvanceGlueVariable (QuantVariableAST 'H.Sym.Tok.GlueQuantity) (QuantVariableTargetAST 'H.Sym.Tok.GlueQuantity)
  | AdvanceMathGlueVariable (QuantVariableAST 'H.Sym.Tok.MathGlueQuantity) (QuantVariableTargetAST 'H.Sym.Tok.MathGlueQuantity)
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
  | WriteMessage MessageWriteCommand
  | ModifyFileStream FileStreamModificationCommand
  | WriteToStream StreamWriteCommand
  | DoSpecial H.Sym.Tok.ExpandedBalancedText
  | AddBox BoxPlacement Box
  | ChangeScope H.Q.Sign CommandTrigger
  deriving stock (Show, Eq, Generic)

data StreamWriteCommand = StreamWriteCommand HexInt WriteText
  deriving stock (Show, Eq, Generic)

data MessageWriteCommand = MessageWriteCommand H.Sym.Tok.StandardOutputStream H.Sym.Tok.ExpandedBalancedText
  deriving stock (Show, Eq, Generic)

data FileStreamModificationCommand = FileStreamModificationCommand FileStreamType FileStreamAction HexInt
  deriving stock (Show, Eq, Generic)

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
