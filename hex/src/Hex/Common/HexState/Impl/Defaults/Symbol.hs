module Hex.Common.HexState.Impl.Defaults.Symbol where

import Data.Map.Strict qualified as Map
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Font
import Hex.Common.HexState.Interface.Grouped
import Hex.Common.HexState.Interface.Parameter
import Hex.Common.HexState.Interface.Register
import Hex.Common.HexState.Interface.Resolve qualified as H.Res
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved (ResolvedToken (..))
import Hex.Common.Token.Resolved.Expandable
import Hex.Common.Token.Resolved.Primitive
import Hex.Stage.Build.ListExtractor.Interface
import Hexlude

_cs :: [Char] -> H.Res.ControlSymbol
_cs =
  H.Res.ControlSequenceSymbol
    . LT.mkControlSequence
    . fmap Code.unsafeCodeFromChar

expandHeadTok :: ExpansionCommandHeadToken -> ResolvedToken
expandHeadTok = ExpansionCommandHeadToken

primTok :: PrimitiveToken -> ResolvedToken
primTok = PrimitiveToken

condTok :: ConditionHeadTok -> ResolvedToken
condTok e = expandHeadTok $ ConditionTok $ ConditionHeadTok e

vModeTok :: ModedCommandPrimitiveToken -> ResolvedToken
vModeTok e = primTok $ ModedCommand Vertical e

hModeTok :: ModedCommandPrimitiveToken -> ResolvedToken
hModeTok e = primTok $ ModedCommand Horizontal e

initialSymbolMap :: H.Res.SymbolMap
initialSymbolMap =
  Map.fromList
    [ (_cs "ifnum", condTok IfIntPairTestTok),
      (_cs "ifdim", condTok IfLengthPairTestTok),
      (_cs "ifodd", condTok IfIntOddTok),
      (_cs "ifvmode", condTok $ IfInModeTok VerticalMode),
      (_cs "ifhmode", condTok $ IfInModeTok HorizontalMode),
      (_cs "ifmmode", condTok $ IfInModeTok MathMode),
      (_cs "ifinner", condTok $ IfInModeTok InnerMode),
      (_cs "if", condTok $ IfTokenAttributesEqualTok CharCodeAttribute),
      (_cs "ifcat", condTok $ IfTokenAttributesEqualTok CatCodeAttribute),
      (_cs "ifx", condTok IfTokensEqualTok),
      (_cs "ifvoid", condTok $ IfBoxRegisterIsTok IsVoid),
      (_cs "ifvbox", condTok $ IfBoxRegisterIsTok HasVerticalBox),
      (_cs "ifhbox", condTok $ IfBoxRegisterIsTok HasHorizontalBox),
      (_cs "ifeof", condTok IfInputEndedTok),
      (_cs "iftrue", condTok $ IfConstTok True),
      (_cs "iffalse", condTok $ IfConstTok False),
      (_cs "ifcase", condTok CaseTok),
      -- Tokens used in the body of condition blocks.
      (_cs "else", expandHeadTok $ ConditionTok $ ConditionBodyTok Else),
      (_cs "or", expandHeadTok $ ConditionTok $ ConditionBodyTok Or),
      (_cs "fi", expandHeadTok $ ConditionTok $ ConditionBodyTok EndIf),
      (_cs "number", expandHeadTok NumberTok),
      (_cs "romannumeral", expandHeadTok RomanNumeralTok),
      (_cs "string", expandHeadTok StringTok),
      (_cs "jobname", expandHeadTok JobNameTok),
      (_cs "fontname", expandHeadTok FontNameTok),
      (_cs "meaning", expandHeadTok MeaningTok),
      (_cs "csname", expandHeadTok CSNameTok),
      (_cs "expandafter", expandHeadTok ExpandAfterTok),
      (_cs "noexpand", expandHeadTok NoExpandTok),
      (_cs "topmark", expandHeadTok $ MarkRegisterTok TopMark),
      (_cs "firstmark", expandHeadTok $ MarkRegisterTok FirstMark),
      (_cs "botmark", expandHeadTok $ MarkRegisterTok BottomMark),
      (_cs "splitfirstmark", expandHeadTok $ MarkRegisterTok SplitFirstMark),
      (_cs "input", expandHeadTok InputTok),
      (_cs "endinput", expandHeadTok EndInputTok),
      (_cs "the", expandHeadTok TheTok),
      (_cs "uppercase", expandHeadTok $ ChangeCaseTok Upward),
      (_cs "lowercase", expandHeadTok $ ChangeCaseTok Downward),
      -- Arguments of expansion-commands.
      (_cs "endcsname", primTok EndCSNameTok),
      -- Nothing special.
      (_cs "relax", primTok RelaxTok),
      (_cs "begingroup", primTok $ ChangeScopeCSTok Q.Positive),
      (_cs "endgroup", primTok $ ChangeScopeCSTok Q.Negative),
      (_cs "show", primTok ShowTokenTok),
      (_cs "showbox", primTok ShowBoxTok),
      (_cs "showlists", primTok ShowListsTok),
      (_cs "showstate", primTok DebugShowState),
      (_cs "showthe", primTok ShowTheInternalQuantityTok),
      (_cs "shipout", primTok ShipOutTok),
      (_cs "ignorespaces", primTok IgnoreSpacesTok),
      (_cs "afterassignment", primTok SetAfterAssignmentTokenTok),
      (_cs "aftergroup", primTok AddToAfterGroupTokensTok),
      (_cs "message", primTok $ MessageTok StdOut),
      (_cs "errmessage", primTok $ MessageTok StdErr),
      (_cs "immediate", primTok ImmediateTok),
      (_cs "openin", primTok OpenInputTok),
      (_cs "closein", primTok CloseInputTok),
      (_cs "openout", primTok OpenOutputTok),
      (_cs "closeout", primTok CloseOutputTok),
      (_cs "write", primTok WriteTok),
      (_cs "special", primTok DoSpecialTok),
      (_cs "penalty", primTok PenaltyTok),
      (_cs "kern", primTok KernTok),
      (_cs "mkern", primTok MathKernTok),
      (_cs "unpenalty", primTok $ RemoveItemTok PenaltyItem),
      (_cs "unkern", primTok $ RemoveItemTok KernItem),
      (_cs "unskip", primTok $ RemoveItemTok GlueItem),
      (_cs "mark", primTok MarkTok),
      (_cs "insert", primTok InsertionTok),
      (_cs "leaders", primTok $ LeadersTok Aligned),
      (_cs "cleaders", primTok $ LeadersTok Centered),
      (_cs "xleaders", primTok $ LeadersTok Expanded),
      (_cs "indent", primTok $ StartParagraphTok Indent),
      (_cs "noindent", primTok $ StartParagraphTok DoNotIndent),
      (_cs "par", primTok EndParagraphTok),
      -- Glue.
      (_cs "vskip", vModeTok SpecifiedGlueTok),
      (_cs "hskip", hModeTok SpecifiedGlueTok),
      (_cs "vfil", vModeTok $ PresetGlueTok Fil),
      (_cs "hfil", hModeTok $ PresetGlueTok Fil),
      (_cs "vfill", vModeTok $ PresetGlueTok Fill),
      (_cs "hfill", hModeTok $ PresetGlueTok Fill),
      (_cs "vfilneg", vModeTok $ PresetGlueTok FilNeg),
      (_cs "hfilneg", hModeTok $ PresetGlueTok FilNeg),
      (_cs "vss", vModeTok $ PresetGlueTok StretchOrShrink),
      (_cs "hss", hModeTok $ PresetGlueTok StretchOrShrink),
      -- Other moded.
      (_cs "halign", hModeTok AlignedMaterialTok),
      (_cs "valign", vModeTok AlignedMaterialTok),
      (_cs "moveleft", hModeTok $ ShiftedBoxTok Backward),
      (_cs "moveright", hModeTok $ ShiftedBoxTok Forward),
      (_cs "raise", vModeTok $ ShiftedBoxTok Backward),
      (_cs "lower", vModeTok $ ShiftedBoxTok Forward),
      (_cs "unvbox", vModeTok $ UnwrappedFetchedBoxTok Pop),
      (_cs "unhbox", hModeTok $ UnwrappedFetchedBoxTok Pop),
      (_cs "unvcopy", vModeTok $ UnwrappedFetchedBoxTok Lookup),
      (_cs "unhcopy", hModeTok $ UnwrappedFetchedBoxTok Lookup),
      (_cs "hrule", vModeTok RuleTok),
      (_cs "vrule", hModeTok RuleTok),
      -- Final commands.
      (_cs "end", primTok EndTok),
      (_cs "dump", primTok DumpTok),
      -- Starters of Horizontal mode commands.
      (_cs " ", primTok ControlSpaceTok),
      (_cs "char", primTok ControlCharTok),
      (_cs "accent", primTok AccentTok),
      (_cs "/", primTok ItalicCorrectionTok),
      (_cs "discretionary", primTok DiscretionaryTextTok),
      (_cs "-", primTok DiscretionaryHyphenTok),
      -- Macro prefixes.
      (_cs "long", primTok $ AssignPrefixTok LongTok),
      (_cs "outer", primTok $ AssignPrefixTok OuterTok),
      (_cs "global", primTok $ AssignPrefixTok GlobalTok),
      -- Macro def types.
      (_cs "def", primTok $ DefineMacroTok Local InhibitDef),
      (_cs "edef", primTok $ DefineMacroTok Local ExpandDef),
      (_cs "gdef", primTok $ DefineMacroTok Global InhibitDef),
      (_cs "xdef", primTok $ DefineMacroTok Global ExpandDef),
      -- Int parameters.
      (_cs "pretolerance", primTok $ IntParamVarTok PreTolerance),
      (_cs "tolerance", primTok $ IntParamVarTok Tolerance),
      (_cs "hbadness", primTok $ IntParamVarTok HBadness),
      (_cs "vbadness", primTok $ IntParamVarTok VBadness),
      (_cs "linepenalty", primTok $ IntParamVarTok LinePenalty),
      (_cs "hyphenpenalty", primTok $ IntParamVarTok HyphenPenalty),
      (_cs "exhyphenpenalty", primTok $ IntParamVarTok ExHyphenPenalty),
      (_cs "binoppenalty", primTok $ IntParamVarTok BinOpPenalty),
      (_cs "relpenalty", primTok $ IntParamVarTok RelPenalty),
      (_cs "clubpenalty", primTok $ IntParamVarTok ClubPenalty),
      (_cs "widowpenalty", primTok $ IntParamVarTok WidowPenalty),
      (_cs "displaywidowpenalty", primTok $ IntParamVarTok DisplayWidowPenalty),
      (_cs "brokenpenalty", primTok $ IntParamVarTok BrokenPenalty),
      (_cs "predisplaypenalty", primTok $ IntParamVarTok PreDisplayPenalty),
      (_cs "postdisplaypenalty", primTok $ IntParamVarTok PostDisplayPenalty),
      (_cs "interlinepenalty", primTok $ IntParamVarTok InterlinePenalty),
      (_cs "floatingpenalty", primTok $ IntParamVarTok FloatingPenalty),
      (_cs "outputpenalty", primTok $ IntParamVarTok OutputPenalty),
      (_cs "doublehyphendemerits", primTok $ IntParamVarTok DoubleHyphenDemerits),
      (_cs "finalhyphendemerits", primTok $ IntParamVarTok FinalHyphenDemerits),
      (_cs "adjdemerits", primTok $ IntParamVarTok AdjDemerits),
      (_cs "looseness", primTok $ IntParamVarTok Looseness),
      (_cs "pausing", primTok $ IntParamVarTok Pausing),
      (_cs "holdinginserts", primTok $ IntParamVarTok HoldingInserts),
      (_cs "tracingonline", primTok $ IntParamVarTok TracingOnline),
      (_cs "tracingmacros", primTok $ IntParamVarTok TracingMacros),
      (_cs "tracingstats", primTok $ IntParamVarTok TracingStats),
      (_cs "tracingparagraphs", primTok $ IntParamVarTok TracingParagraphs),
      (_cs "tracingpages", primTok $ IntParamVarTok TracingPages),
      (_cs "tracingoutput", primTok $ IntParamVarTok TracingOutput),
      (_cs "tracinglostchars", primTok $ IntParamVarTok TracingLostChars),
      (_cs "tracingcommands", primTok $ IntParamVarTok TracingCommands),
      (_cs "tracingrestores", primTok $ IntParamVarTok TracingRestores),
      (_cs "language", primTok $ IntParamVarTok Language),
      (_cs "uchyph", primTok $ IntParamVarTok UCHyph),
      (_cs "lefthyphenmin", primTok $ IntParamVarTok LeftHyphenMin),
      (_cs "righthyphenmin", primTok $ IntParamVarTok RightHyphenMin),
      (_cs "globaldefs", primTok $ IntParamVarTok GlobalDefs),
      (_cs "defaulthyphenchar", primTok $ IntParamVarTok DefaultHyphenChar),
      (_cs "defaultskewchar", primTok $ IntParamVarTok DefaultSkewChar),
      (_cs "escapechar", primTok $ IntParamVarTok EscapeChar),
      (_cs "endlinechar", primTok $ IntParamVarTok EndLineChar),
      (_cs "newlinechar", primTok $ IntParamVarTok NewLineChar),
      (_cs "maxdeadcycles", primTok $ IntParamVarTok MaxDeadCycles),
      (_cs "hangafter", primTok $ IntParamVarTok HangAfter),
      (_cs "fam", primTok $ IntParamVarTok Fam),
      (_cs "mag", primTok $ IntParamVarTok Mag),
      (_cs "delimiterfactor", primTok $ IntParamVarTok DelimiterFactor),
      (_cs "time", primTok $ IntParamVarTok Time),
      (_cs "day", primTok $ IntParamVarTok Day),
      (_cs "month", primTok $ IntParamVarTok Month),
      (_cs "year", primTok $ IntParamVarTok Year),
      (_cs "showboxbreadth", primTok $ IntParamVarTok ShowBoxBreadth),
      (_cs "showboxdepth", primTok $ IntParamVarTok ShowBoxDepth),
      (_cs "errorcontextlines", primTok $ IntParamVarTok ErrorContextLines),
      -- Length parameters.
      (_cs "hfuzz", primTok $ LengthParamVarTok HFuzz),
      (_cs "vfuzz", primTok $ LengthParamVarTok VFuzz),
      (_cs "overfullrule", primTok $ LengthParamVarTok OverfullRule),
      (_cs "emergencystretch", primTok $ LengthParamVarTok EmergencyStretch),
      (_cs "hsize", primTok $ LengthParamVarTok HSize),
      (_cs "vsize", primTok $ LengthParamVarTok VSize),
      (_cs "maxdepth", primTok $ LengthParamVarTok MaxDepth),
      (_cs "splitmaxdepth", primTok $ LengthParamVarTok SplitMaxDepth),
      (_cs "boxmaxdepth", primTok $ LengthParamVarTok BoxMaxDepth),
      (_cs "lineskiplimit", primTok $ LengthParamVarTok LineSkipLimit),
      (_cs "delimitershortfall", primTok $ LengthParamVarTok DelimiterShortfall),
      (_cs "nulldelimiterspace", primTok $ LengthParamVarTok NullDelimiterSpace),
      (_cs "scriptspace", primTok $ LengthParamVarTok ScriptSpace),
      (_cs "mathsurround", primTok $ LengthParamVarTok MathSurround),
      (_cs "predisplaysize", primTok $ LengthParamVarTok PreDisplaySize),
      (_cs "displaywidth", primTok $ LengthParamVarTok DisplayWidth),
      (_cs "displayindent", primTok $ LengthParamVarTok DisplayIndent),
      (_cs "parindent", primTok $ LengthParamVarTok ParIndent),
      (_cs "hangindent", primTok $ LengthParamVarTok HangIndent),
      (_cs "hoffset", primTok $ LengthParamVarTok HOffset),
      (_cs "voffset", primTok $ LengthParamVarTok VOffset),
      -- Glue parameters.
      (_cs "baselineskip", primTok $ GlueParamVarTok BaselineSkip),
      (_cs "lineskip", primTok $ GlueParamVarTok LineSkip),
      (_cs "parskip", primTok $ GlueParamVarTok ParSkip),
      (_cs "abovedisplayskip", primTok $ GlueParamVarTok AboveDisplaySkip),
      (_cs "abovedisplayshortskip", primTok $ GlueParamVarTok AboveDisplayShortSkip),
      (_cs "belowdisplayskip", primTok $ GlueParamVarTok BelowDisplaySkip),
      (_cs "belowdisplayshortskip", primTok $ GlueParamVarTok BelowDisplayShortSkip),
      (_cs "leftskip", primTok $ GlueParamVarTok LeftSkip),
      (_cs "rightskip", primTok $ GlueParamVarTok RightSkip),
      (_cs "topskip", primTok $ GlueParamVarTok TopSkip),
      (_cs "splittopskip", primTok $ GlueParamVarTok SplitTopSkip),
      (_cs "tabskip", primTok $ GlueParamVarTok TabSkip),
      (_cs "spaceskip", primTok $ GlueParamVarTok SpaceSkip),
      (_cs "xspaceskip", primTok $ GlueParamVarTok XSpaceSkip),
      (_cs "parfillskip", primTok $ GlueParamVarTok ParFillSkip),
      -- Math-glue parameters.
      (_cs "thinmuskip", primTok $ MathGlueParamVarTok ThinMuSkip),
      (_cs "medmuskip", primTok $ MathGlueParamVarTok MedMuSkip),
      (_cs "thickmuskip", primTok $ MathGlueParamVarTok ThickMuSkip),
      -- Token list parameters.
      (_cs "output", primTok $ TokenListParamVarTok Output),
      (_cs "everypar", primTok $ TokenListParamVarTok EveryPar),
      (_cs "everymath", primTok $ TokenListParamVarTok EveryMath),
      (_cs "everydisplay", primTok $ TokenListParamVarTok EveryDisplay),
      (_cs "everyhbox", primTok $ TokenListParamVarTok EveryHBox),
      (_cs "everyvbox", primTok $ TokenListParamVarTok EveryVBox),
      (_cs "everyjob", primTok $ TokenListParamVarTok EveryJob),
      (_cs "everycr", primTok $ TokenListParamVarTok EveryCR),
      (_cs "errhelp", primTok $ TokenListParamVarTok ErrHelp),
      -- Special integers.
      (_cs "spacefactor", primTok $ SpecialIntParameterTok SpaceFactor),
      (_cs "prevgraf", primTok $ SpecialIntParameterTok PrevGraf),
      (_cs "deadcycles", primTok $ SpecialIntParameterTok DeadCycles),
      (_cs "insertpenalties", primTok $ SpecialIntParameterTok InsertPenalties),
      -- Special lengths.
      (_cs "prevdepth", primTok $ SpecialLengthParameterTok PrevDepth),
      (_cs "pagegoal", primTok $ SpecialLengthParameterTok PageGoal),
      (_cs "pagetotal", primTok $ SpecialLengthParameterTok PageTotal),
      (_cs "pagestretch", primTok $ SpecialLengthParameterTok PageStretch),
      (_cs "pagefilstretch", primTok $ SpecialLengthParameterTok PageFilStretch),
      (_cs "pagefillstretch", primTok $ SpecialLengthParameterTok PageFillStretch),
      (_cs "pagefilllstretch", primTok $ SpecialLengthParameterTok PageFilllStretch),
      (_cs "pageshrink", primTok $ SpecialLengthParameterTok PageShrink),
      (_cs "pagedepth", primTok $ SpecialLengthParameterTok PageDepth),
      -- Register reference type prefixes.
      (_cs "count", primTok $ RegisterVariableTok Q.IntQuantity),
      (_cs "dimen", primTok $ RegisterVariableTok Q.LengthQuantity),
      (_cs "skip", primTok $ RegisterVariableTok Q.GlueQuantity),
      (_cs "muskip", primTok $ RegisterVariableTok Q.MathGlueQuantity),
      (_cs "toks", primTok $ RegisterVariableTok Q.TokenListQuantity),
      -- Short-hand definition heads.
      (_cs "chardef", primTok $ ShortDefHeadTok CharQuantity),
      (_cs "mathchardef", primTok $ ShortDefHeadTok MathCharQuantity),
      (_cs "countdef", primTok $ ShortDefHeadTok $ QuantityType Q.IntQuantity),
      (_cs "dimendef", primTok $ ShortDefHeadTok $ QuantityType Q.LengthQuantity),
      (_cs "skipdef", primTok $ ShortDefHeadTok $ QuantityType Q.GlueQuantity),
      (_cs "muskipdef", primTok $ ShortDefHeadTok $ QuantityType Q.MathGlueQuantity),
      (_cs "toksdef", primTok $ ShortDefHeadTok $ QuantityType Q.TokenListQuantity),
      -- Modify variables.
      (_cs "advance", primTok AdvanceVarTok),
      (_cs "multiply", primTok $ ScaleVarTok Upward),
      (_cs "divide", primTok $ ScaleVarTok Downward),
      -- Code types.
      (_cs "catcode", primTok $ CodeTypeTok Code.CatCodeType),
      (_cs "mathcode", primTok $ CodeTypeTok Code.MathCodeType),
      (_cs "lccode", primTok $ CodeTypeTok $ Code.LowerCaseCodeType),
      (_cs "uccode", primTok $ CodeTypeTok $ Code.UpperCaseCodeType),
      (_cs "sfcode", primTok $ CodeTypeTok Code.SpaceFactorCodeType),
      (_cs "delcode", primTok $ CodeTypeTok Code.DelimiterCodeType),
      -- Alias tokens.
      (_cs "let", primTok LetTok),
      (_cs "futurelet", primTok FutureLetTok),
      -- Font range.
      (_cs "textfont", primTok $ FontRangeTok TextSizeFontRange),
      (_cs "scriptfont", primTok $ FontRangeTok ScriptSizeFontRange),
      (_cs "scriptscriptfont", primTok $ FontRangeTok ScriptScriptSizeFontRange),
      -- Internal integer.
      (_cs "lastpenalty", primTok LastPenaltyTok),
      (_cs "parshape", primTok ParagraphShapeTok),
      (_cs "badness", primTok BadnessTok),
      (_cs "inputlineno", primTok InputLineNrTok),
      -- Internal length.
      (_cs "lastkern", primTok LastKernTok),
      (_cs "fontdimen", primTok FontDimensionTok),
      (_cs "ht", primTok $ BoxDimensionTok Box.BoxHeight),
      (_cs "wd", primTok $ BoxDimensionTok Box.BoxWidth),
      (_cs "dp", primTok $ BoxDimensionTok Box.BoxDepth),
      -- Internal glue.
      (_cs "lastskip", primTok LastGlueTok),
      -- Specifying a box.
      (_cs "box", primTok $ FetchedBoxTok Pop),
      (_cs "copy", primTok $ FetchedBoxTok Lookup),
      (_cs "lastbox", primTok LastBoxTok),
      (_cs "vsplit", primTok SplitVBoxTok),
      (_cs "hbox", primTok $ ExplicitBoxTok ExplicitHBoxType),
      (_cs "vbox", primTok $ ExplicitBoxTok $ ExplicitVBoxType DefaultAlign),
      (_cs "vtop", primTok $ ExplicitBoxTok $ ExplicitVBoxType TopAlign),
      (_cs "setbox", primTok SetBoxRegisterTok),
      -- Stream.
      (_cs "read", primTok ReadTok),
      -- Font.
      (_cs "font", primTok FontTok),
      (_cs "hyphenchar", primTok $ FontSpecialCharTok HyphenChar),
      (_cs "skewchar", primTok $ FontSpecialCharTok SkewChar),
      -- Hyphenation.
      (_cs "hyphenation", primTok HyphenationTok),
      (_cs "patterns", primTok HyphenationPatternsTok),
      -- Interaction mode.
      (_cs "errorstopmode", primTok $ InteractionModeTok ErrorStopMode),
      (_cs "scrollmode", primTok $ InteractionModeTok ScrollMode),
      (_cs "nonstopmode", primTok $ InteractionModeTok NonStopMode),
      (_cs "batchmode", primTok $ InteractionModeTok BatchMode)
    ]
