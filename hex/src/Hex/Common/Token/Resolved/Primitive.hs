module Hex.Common.Token.Resolved.Primitive where

import ASCII qualified
import ASCII.Char qualified
import Formatting qualified as F
import Hex.Common.Codes
import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Register
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Stage.Build.ListExtractor.Interface qualified as ListExtractor
import Hexlude

data AssignPrefixTok
  = LongTok
  | OuterTok
  | GlobalTok
  deriving stock (Show, Eq, Generic)

data ExpandDefFlag
  = ExpandDef
  | InhibitDef
  deriving stock (Show, Eq, Generic)

data StandardOutputInput
  = StdOut
  | StdErr
  deriving stock (Show, Eq, Generic)

data PresetGlueType
  = Fil -- \{h,v}fil
  | Fill -- \{h,v}fill
  | StretchOrShrink -- \{h,v}ss
  | FilNeg -- \{h,v}filneg
  deriving stock (Show, Eq, Generic)

data LeadersType
  = Aligned -- \leaders
  | Centered -- \cleaders
  | Expanded -- \xleaders
  deriving stock (Show, Eq, Generic)

data ModeAttribute
  = VerticalMode
  | HorizontalMode
  | MathMode
  | InnerMode
  deriving stock (Show, Eq, Generic)

data BoxRegisterAttribute
  = HasVerticalBox
  | HasHorizontalBox
  | IsVoid
  deriving stock (Show, Eq, Generic)

data ModedCommandPrimitiveToken
  = SpecifiedGlueTok -- \vskip, \hskip
  | PresetGlueTok PresetGlueType -- \{v,h}{fil,fill,filneg,ss}
  | AlignedMaterialTok -- \halign, \valign
  | ShiftedBoxTok Q.Direction -- \moveleft, \moveright, \raise, \lower
  | UnwrappedFetchedBoxTok HSt.Register.BoxFetchMode -- \un{v,h}{box,copy}
  | RuleTok -- \hrule, \vrule
  deriving stock (Show, Eq, Generic)

data CharryQuantityType
  = CharQuantity -- \chardef
  | MathCharQuantity -- \mathchardef
  | QuantityType Q.QuantityType
  deriving stock (Show, Eq, Generic)

data NumericQuantityType
  = IntNumericQuantity
  | LengthNumericQuantity
  | GlueNumericQuantity
  | MathGlueNumericQuantity
  deriving stock (Show, Eq, Generic)

data InteractionMode
  = ErrorStopMode -- \errorstopmode
  | ScrollMode -- \scrollmode
  | NonStopMode -- \nonstopmode
  | BatchMode -- \batchmode
  deriving stock (Show, Eq, Generic)

data NonZeroDigit
  = Digit1
  | Digit2
  | Digit3
  | Digit4
  | Digit5
  | Digit6
  | Digit7
  | Digit8
  | Digit9
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)

digitToChar :: NonZeroDigit -> CharCode
digitToChar d =
  CharCode $ case d of
    Digit1 -> ASCII.charToWord8 ASCII.Char.Digit1
    Digit2 -> ASCII.charToWord8 ASCII.Char.Digit2
    Digit3 -> ASCII.charToWord8 ASCII.Char.Digit3
    Digit4 -> ASCII.charToWord8 ASCII.Char.Digit4
    Digit5 -> ASCII.charToWord8 ASCII.Char.Digit5
    Digit6 -> ASCII.charToWord8 ASCII.Char.Digit6
    Digit7 -> ASCII.charToWord8 ASCII.Char.Digit7
    Digit8 -> ASCII.charToWord8 ASCII.Char.Digit8
    Digit9 -> ASCII.charToWord8 ASCII.Char.Digit9

charCodeToDigit :: CharCode -> Maybe NonZeroDigit
charCodeToDigit cc = case unsafeCodeAsChar cc of
  '1' -> Just Digit1
  '2' -> Just Digit2
  '3' -> Just Digit3
  '4' -> Just Digit4
  '5' -> Just Digit5
  '6' -> Just Digit6
  '7' -> Just Digit7
  '8' -> Just Digit8
  '9' -> Just Digit9
  _ -> Nothing

data RemovableItem
  = PenaltyItem -- \unpenalty
  | KernItem -- \unkern
  | GlueItem -- \unskip
  deriving stock (Show, Eq, Generic)

data ExplicitBoxType
  = ExplicitHBoxType -- \hbox
  | ExplicitVBoxType VBoxAlignType
  deriving stock (Show, Eq, Generic)

data VBoxAlignType
  = DefaultAlign -- \vbox
  | TopAlign -- \vtop
  deriving stock (Show, Eq, Generic)

data ShortDefTargetValue = ShortDefTargetValue CharryQuantityType Q.HexInt
  deriving stock (Show, Eq, Generic)

data PrimitiveToken
  = EndCSNameTok -- \endcsname
  | -- Starters of commands.
    RelaxTok -- \relax
  | ChangeScopeCSTok Q.Sign
  | ShowTokenTok -- \show
  | ShowBoxTok -- \showbox
  | ShowListsTok -- \showlists
  | ShowTheInternalQuantityTok -- \showthe
  | ShipOutTok -- \shipout
  | IgnoreSpacesTok -- \ignorespaces
  | SetAfterAssignmentTokenTok -- \afterassignment
  | AddToAfterGroupTokensTok -- \aftergroup
  | MessageTok StandardOutputInput -- \message, \errmessage
  | ImmediateTok -- \immediate
  | OpenInputTok -- \openin
  | CloseInputTok -- \closein
  | OpenOutputTok -- \openout
  | CloseOutputTok -- \closeout
  | WriteTok -- \write
  | DoSpecialTok -- \special
  | PenaltyTok -- \penalty
  | KernTok -- \kern
  | MathKernTok -- \mkern
  | RemoveItemTok RemovableItem
  | MarkTok -- \mark
  | InsertionTok -- \insert
  | LeadersTok LeadersType
  | StartParagraphTok ListExtractor.IndentFlag -- \indent, \noindent
  | EndParagraphTok -- \par
  -- Starters of mode-specific commands with almost mode-independent grammar.
  | ModedCommand Q.Axis ModedCommandPrimitiveToken
  | -- Starters of Vertical-Mode-specific commands.
    EndTok -- \end
  | DumpTok -- \dump
  -- Starters of Horizontal-Mode-specific commands.
  | ControlSpaceTok -- \␣ (a control symbol named ' ')
  | ControlCharTok -- \char
  | AccentTok -- \accent
  | ItalicCorrectionTok -- \/
  | DiscretionaryTextTok -- \discretionary
  | DiscretionaryHyphenTok -- \-
  | ToggleMathModeTok -- '$'
  -- > > Modifying how to apply assignments.
  | AssignPrefixTok AssignPrefixTok
  | -- > > Modifying how to parse the macro.
    --     \def, \gdef, \edef (expanded-def), \xdef (global-expanded-def).
    DefineMacroTok HSt.Grouped.ScopeFlag ExpandDefFlag
  | -- > Setting variable values.
    IntParamVarTok HSt.Param.IntParameter
  | LengthParamVarTok HSt.Param.LengthParameter
  | GlueParamVarTok HSt.Param.GlueParameter
  | MathGlueParamVarTok HSt.Param.MathGlueParameter
  | TokenListParamVarTok HSt.Param.TokenListParameter
  | SpecialIntParameterTok HSt.Param.SpecialIntParameter -- \example: \spacefactor
  | SpecialLengthParameterTok HSt.Param.SpecialLengthParameter -- \example: \pagestretch
  -- Tokens storing integers defined by short-hand definitions.
  | ShortDefTargetToken ShortDefTargetValue
  | -- A char-cat pair defined by a 'let' assignment. This differs from a
    -- \chardef target, because \chardef maps to a character number, which is
    -- categorised at the time of use, while a \let maps to a static char-cat
    -- pair.
    LetCharCat LT.LexCharCat
  | -- A control sequence representing a particular font, such as defined through
    -- \font.
    FontRefToken HSt.Font.FontNumber
  | -- Heads of register references.
    RegisterVariableTok Q.QuantityType
  | -- Heads of int-ref definitions.
    ShortDefHeadTok CharryQuantityType
  | -- > Modifying variable values with arithmetic.
    AdvanceVarTok -- \advance
  | ScaleVarTok Q.VDirection -- \multiply, \divide.
  | CodeTypeTok CodeType
  | -- > Aliasing tokens.
    LetTok -- \let
  | FutureLetTok -- \futurelet
  -- > Setting font math-family-member things.
  | FontRangeTok HSt.Font.FontRange
  | -- > Internal integers.
    LastPenaltyTok -- \lastpenalty
  | ParagraphShapeTok -- \parshape
  | BadnessTok -- \badness
  | InputLineNrTok -- \inputlineno
  -- Internal lengths.
  | LastKernTok -- \lastkern
  | FontDimensionTok -- \fontdimen
  | BoxDimensionTok Q.BoxDim -- \ht, \wd, \dp
  -- Internal glues.
  | LastGlueTok -- \lastskip
  -- Specifying boxes.
  | FetchedBoxTok HSt.Register.BoxFetchMode -- \box, \copy
  | LastBoxTok -- \lastbox
  | SplitVBoxTok -- \vsplit
  | ExplicitBoxTok ExplicitBoxType
  | -- > Setting the contents of a box register.
    SetBoxRegisterTok -- \setbox
    -- > Reading contents into control sequences (not sure what this is about).
  | ReadTok -- \read
  -- > Defining macros resolving to a font.
  | FontTok -- \font
  -- Involved in global assignments.
  -- > Setting properties of a font.
  | FontSpecialCharTok HSt.Font.FontSpecialChar
  | -- > Configuring hyphenation.
    HyphenationTok -- \hyphenation
  | HyphenationPatternsTok -- \patterns
  -- > Setting interaction mode.
  | InteractionModeTok InteractionMode
  | CharCatPair LT.LexCharCat
  | -- Custom token for my own use.
    DebugShowState
  deriving stock (Show, Eq, Generic)

primTokCharCat :: Prism' PrimitiveToken LT.LexCharCat
primTokCharCat = _Ctor @"CharCatPair"

fmtPrimitiveToken :: Fmt PrimitiveToken
fmtPrimitiveToken = F.later $ \case
  CharCatPair cc -> F.bformat ("CharCatPair" |%| F.parenthesised LT.fmtLexCharCat) cc
  t -> F.bformat F.shown t

ccp :: Char -> CoreCatCode -> PrimitiveToken
ccp c cat = CharCatPair $ LT.lcc c cat
