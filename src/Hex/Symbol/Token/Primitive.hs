module Hex.Symbol.Token.Primitive where

import ASCII qualified
import ASCII.Char qualified
import Hex.Codes
import Hex.Lex.Types qualified as H.Lex
import Hex.Quantity qualified as H.Q
import Hexlude

data IntParameter
  = PreTolerance -- Badness tolerance before hyphenation
  | Tolerance -- Badness tolerance after hyphenation
  | HBadness -- Badness above which bad hboxes will be shown
  | VBadness -- Badness above which bad vboxes will be shown
  | LinePenalty -- Amount added to badness of every line in a paragraph
  | HyphenPenalty -- Penalty for line break after discretionary hyphen
  | ExHyphenPenalty -- Penalty for line break after explicit hyphen
  | BinOpPenalty -- Penalty for line break after binary operation
  | RelPenalty -- Penalty for line break after math relation
  | ClubPenalty -- Penalty for creating a club line at bottom of page
  | WidowPenalty -- Penalty for creating a widow line at top of page
  | DisplayWidowPenalty -- Ditto, before a display
  | BrokenPenalty -- Penalty for page break after a hyphenated line
  | PreDisplayPenalty -- Penalty for page break just before a display
  | PostDisplayPenalty -- Penalty for page break just after a display
  | InterlinePenalty -- Additional penalty for page break between lines
  | FloatingPenalty -- Penalty for insertions that are split
  | OutputPenalty -- Penalty at the current page break
  | DoubleHyphenDemerits -- Demerits for consecutive broken lines
  | FinalHyphenDemerits -- Demerits for a penultimate broken line
  | AdjDemerits -- Demerits for adjacent incompatible lines
  | Looseness -- Change to the number of lines in a paragraph
  | Pausing -- Positive if pausing after each line is read from a file
  | HoldingInserts -- Positive if insertions remain dormant in output box
  | TracingOnline -- Positive if showing diagnostic info on the terminal
  | TracingMacros -- Positive if showing macros as they are expanded
  | TracingStats -- Positive if showing statistics about memory usage
  | TracingParagraphs -- Positive if showing line-break calculations
  | TracingPages -- Positive if showing page-break calculations
  | TracingOutput -- Positive if showing boxes that are shipped out
  | TracingLostChars -- Positive if showing characters not in the font
  | TracingCommands -- Positive if showing commands before they are executed
  | TracingRestores -- Positive if showing deassignments when groups end
  | Language -- The current set of hyphenation rules
  | UCHyph -- Positive if hyphenating words beginning with capital letters
  | LeftHyphenMin -- Smallest fragment at beginning of hyphenated word
  | RightHyphenMin -- Smallest fragment at end of hyphenated word
  | GlobalDefs -- Nonzero if overriding \global specifications
  | DefaultHyphenChar -- \hyphenchar value when a font is loaded
  | DefaultSkewChar -- \skewchar value when a font is loaded
  | EscapeChar -- Escape character in the output of control-sequence tokens
  | EndLineChar -- Character placed at the right end of an input line
  | NewLineChar -- Character that starts a new output line
  | MaxDeadCycles -- Upper bound on \deadcycles
  | HangAfter -- Hanging indentation changes after this many lines
  | Fam -- The current family number
  | Mag -- Magnification ratio, times 1000
  | DelimiterFactor -- Ratio for variable delimiters, times 1000
  | Time -- Current time of day in minutes since midnight
  | Day -- Current day of the month
  | Month -- Current month of the year
  | Year -- Current year of our Lord
  | ShowBoxBreadth -- Maximum items per level when boxes are shown
  | ShowBoxDepth -- Maximum level when boxes are shown
  | ErrorContextLines -- Maximum extra context shown when errors occur
  deriving stock (Show, Eq, Ord, Generic)

data LengthParameter
  = HFuzz -- Maximum overrun before overfull hbox messages occur
  | VFuzz -- Maximum overrun before overfull vbox messages occur
  | OverfullRule -- Width of rules appended to overfull boxes
  | EmergencyStretch -- Reduces badnesses on final pass of line-breaking
  | HSize -- Line width in horizontal mode
  | VSize -- Page height in vertical mode
  | MaxDepth -- Maximum depth of boxes on main pages
  | SplitMaxDepth -- Maximum depth of boxes on split pages
  | BoxMaxDepth -- Maximum depth of boxes on explicit pages
  | LineSkipLimit -- Threshold where \baselineskip changes to \lineskip
  | DelimiterShortfall -- Maximum space not covered by a delimiter
  | NullDelimiterSpace -- Width of a null delimiter
  | ScriptSpace -- Extra space after subscript or superscript
  | MathSurround -- Kerning before and after math in text
  | PreDisplaySize -- Length of text preceding a display
  | DisplayWidth -- Length of line for displayed equation
  | DisplayIndent -- Indentation of line for displayed equation
  | ParIndent -- Width of \indent
  | HangIndent -- Amount of hanging indentation
  | HOffset -- Horizontal offset in \shipout
  | VOffset -- Vertical offset in \shipout
  deriving stock (Show, Eq, Ord, Generic)

data GlueParameter
  = BaselineSkip -- Desired glue between baselines
  | LineSkip -- Interline glue if \baselineskip isn't feasible
  | ParSkip -- Extra glue just above paragraphs
  | AboveDisplaySkip -- Extra glue just above displays
  | AboveDisplayShortSkip -- Ditto, following short lines
  | BelowDisplaySkip -- Extra glue just below Displays
  | BelowDisplayShortSkip -- Ditto, following short lines
  | LeftSkip -- Glue at left of justified lines
  | RightSkip -- Glue at right of justified lines
  | TopSkip -- Glue at top of main pages
  | SplitTopSkip -- Glue at top of split pages
  | TabSkip -- Glue between aligned entries
  | SpaceSkip -- Glue between words, if nonzero
  | XSpaceSkip -- Glue between sentences, if nonzero
  | ParFillSkip -- Additional \rightskip at end of paragraphs
  deriving stock (Show, Eq, Ord, Generic)

data MathGlueParameter
  = ThinMuSkip -- Thin space in math formulas
  | MedMuSkip -- Medium space in math formulas
  | ThickMuSkip -- Thick space in math formulas
  deriving stock (Show, Eq, Ord, Generic)

data TokenListParameter
  = Output -- The user's output routine
  | EveryPar -- Tokens to insert when a paragraph begins
  | EveryMath -- Tokens to insert when math in text begins
  | EveryDisplay -- Tokens to insert when display math begins
  | EveryHBox -- Tokens to insert when an hbox begins
  | EveryVBox -- Tokens to insert when a vbox begins
  | EveryJob -- Tokens to insert when the job begins
  | EveryCR -- Tokens to insert after every \cr or nonredundant \crcr
  | ErrHelp -- Tokens that supplement an \errmessage
  deriving stock (Show, Eq, Ord, Generic)

data SpecialIntParameter
  = SpaceFactorHexInt
  | PrevGrafHexInt
  | DeadCyclesHexInt
  | InsertPenaltiesHexInt
  deriving stock (Show, Eq, Ord, Generic)

data SpecialLengthParameter
  = PrevDepth
  | PageGoal
  | PageTotal
  | PageStretch
  | PageFilStretch
  | PageFillStretch
  | PageFilllStretch
  | PageShrink
  | PageDepth
  deriving stock (Show, Eq, Ord, Generic)

data AssignPrefixTok
  = LongTok
  | OuterTok
  | GlobalTok
  deriving stock (Show, Eq, Generic)

data IndentFlag
  = Indent
  | DoNotIndent
  deriving stock (Show, Eq, Generic)

data ExpandDefFlag
  = ExpandDef
  | InhibitDef
  deriving stock (Show, Eq, Generic)

data ScopeFlag
  = Global
  | Local
  deriving stock (Show, Eq, Generic)

data StandardOutputStream
  = StdOut
  | StdErr
  deriving stock (Show, Eq, Generic)

data BoxFetchMode
  = Pop
  | Lookup
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

data FontChar
  = HyphenChar -- \hyphenchar
  | SkewChar -- \skewchar
  deriving stock (Show, Eq, Generic)

data FontRange
  = TextSizeFontRange -- \textfont
  | ScriptSizeFontRange -- \scriptfont
  | ScriptScriptSizeFontRange -- \scriptscriptfont
  deriving stock (Show, Eq, Generic)

instance Hashable FontRange

data ModedCommandPrimitiveToken
  = SpecifiedGlueTok -- \vskip, \hskip
  | PresetGlueTok PresetGlueType -- \{v,h}{fil,fill,filneg,ss}
  | AlignedMaterialTok -- \halign, \valign
  | ShiftedBoxTok H.Q.Direction -- \moveleft, \moveright, \raise, \lower
  | UnwrappedFetchedBoxTok BoxFetchMode -- \un{v,h}{box,copy}
  | RuleTok -- \hrule, \vrule
  deriving stock (Show, Eq, Generic)

data SyntaxCommandArg
  = EndCSNameTok -- \endcsname
  deriving stock (Show, Eq, Generic)

data CodeType
  = CategoryCodeType
  | MathCodeType
  | ChangeCaseCodeType H.Q.VDirection
  | SpaceFactorCodeType
  | DelimiterCodeType
  deriving stock (Show, Eq, Generic)

data CharryQuantityType
  = CharQuantity -- \chardef
  | MathCharQuantity -- \mathchardef
  | QuantityType QuantityType
  deriving stock (Show, Eq, Generic)

data QuantityType
  = IntQuantity -- \count, \countdef
  | LengthQuantity -- \dimen, \dimendef
  | GlueQuantity -- \skip, \skipdef
  | MathGlueQuantity -- \muskip, \muskipdef
  | TokenListQuantity -- \toks, \toksdef
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

data ExplicitBox
  = ExplicitHBox -- \hbox
  | ExplicitVBox VBoxAlignType
  deriving stock (Show, Eq, Generic)

data VBoxAlignType
  = DefaultAlign -- \vbox
  | TopAlign -- \vtop
  deriving stock (Show, Eq, Generic)

newtype FontNumber = FontNumber H.Q.HexInt
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

data PrimitiveToken
  = SyntaxCommandArg SyntaxCommandArg
  | -- Starters of commands.
    RelaxTok -- \relax
  | ChangeScopeCSTok H.Q.Sign
  | ShowTokenTok -- \show
  | ShowBoxTok -- \showbox
  | ShowListsTok -- \showlists
  | ShowTheInternalQuantityTok -- \showthe
  | ShipOutTok -- \shipout
  | IgnoreSpacesTok -- \ignorespaces
  | SetAfterAssignmentTokenTok -- \afterassignment
  | AddToAfterGroupTokensTok -- \aftergroup
  | MessageTok StandardOutputStream -- \message, \errmessage
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
  | StartParagraphTok IndentFlag -- \indent, \noindent
  | EndParagraphTok -- \par
  -- Starters of mode-specific commands with almost mode-independent grammar.
  | ModedCommand H.Q.Axis ModedCommandPrimitiveToken
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
    DefineMacroTok ScopeFlag ExpandDefFlag
  | -- > Setting variable values.
    IntParamVarTok IntParameter
  | LengthParamVarTok LengthParameter
  | GlueParamVarTok GlueParameter
  | MathGlueParamVarTok MathGlueParameter
  | TokenListParamVarTok TokenListParameter
  | SpecialIntParameterTok SpecialIntParameter -- \example: \spacefactor
  | SpecialLengthParameterTok SpecialLengthParameter -- \example: \pagestretch
  -- Tokens storing integers defined by short-hand definitions.
  | IntRefTok CharryQuantityType H.Q.HexInt
  | -- A char-cat pair defined by a 'let' assignment. This differs from a
    -- \chardef target, because \chardef maps to a character number, which is
    -- categorised at the time of use, while a \let maps to a static char-cat
    -- pair.
    LetCharCat H.Lex.LexCharCat
  | -- A control sequence representing a particular font, such as defined through
    -- \font.
    FontRefToken FontNumber
  | -- Heads of register references.
    RegisterVariableTok QuantityType
  | -- Heads of int-ref definitions.
    ShortDefHeadTok CharryQuantityType
  | -- > Modifying variable values with arithmetic.
    AdvanceVarTok -- \advance
  | ScaleVarTok H.Q.VDirection -- \multiply, \divide.
  | CodeTypeTok CodeType
  | -- > Aliasing tokens.
    LetTok -- \let
  | FutureLetTok -- \futurelet
  -- > Setting font math-family-member things.
  | FontRangeTok FontRange
  | -- > Internal integers.
    LastPenaltyTok -- \lastpenalty
  | ParagraphShapeTok -- \parshape
  | BadnessTok -- \badness
  | InputLineNrTok -- \inputlineno
  -- Internal lengths.
  | LastKernTok -- \lastkern
  | FontDimensionTok -- \fontdimen
  | BoxDimensionTok H.Q.BoxDim -- \ht, \wd, \dp
  -- Internal glues.
  | LastGlueTok -- \lastskip
  -- Specifying boxes.
  | FetchedBoxTok BoxFetchMode -- \box, \copy
  | LastBoxTok -- \lastbox
  | SplitVBoxTok -- \vsplit
  | ExplicitBoxTok ExplicitBox
  | -- > Setting the contents of a box register.
    SetBoxRegisterTok -- \setbox
    -- > Reading contents into control sequences (not sure what this is about).
  | ReadTok -- \read
  -- > Defining macros resolving to a font.
  | FontTok -- \font
  -- Involved in global assignments.
  -- > Setting properties of a font.
  | FontCharTok FontChar
  | -- > Configuring hyphenation.
    HyphenationTok -- \hyphenation
  | HyphenationPatternsTok -- \patterns
  -- > Setting interaction mode.
  | InteractionModeTok InteractionMode
  | UnresolvedTok H.Lex.LexToken
  deriving stock (Show, Eq, Generic)
