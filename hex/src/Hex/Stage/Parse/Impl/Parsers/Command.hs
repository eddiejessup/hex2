module Hex.Stage.Parse.Impl.Parsers.Command where

import Control.Monad.Combinators qualified as PC
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Box qualified as Box
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Group
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (PrimTokenSource (..), parseFail)
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Command.Assignment qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Box qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Stream qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Glue qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.MathLength qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

parseCommand :: [PrimTokenSource, NonDet, Log.HexLog] :>> es => Eff es AST.Command
parseCommand =
  anyPrim >>= \case
    PT.DebugShowState ->
      pure $ AST.ModeIndependentCommand $ AST.DebugShowState
    PT.ShowTokenTok ->
      AST.ModeDependentCommand . AST.ShowToken <$> anyLexInhibited
    PT.ShowBoxTok ->
      AST.ModeDependentCommand . AST.ShowBox <$> Par.parseInt
    PT.ShowListsTok ->
      pure $ AST.ModeDependentCommand AST.ShowLists
    PT.ShowTheInternalQuantityTok ->
      AST.ModeDependentCommand . AST.ShowTheInternalQuantity <$> (anyPrim >>= headToParseInternalQuantity)
    PT.ShipOutTok ->
      AST.ModeDependentCommand . AST.ShipOut <$> (anyPrim >>= Par.headToParseBox)
    PT.MarkTok ->
      AST.ModeDependentCommand . AST.AddMark <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
    PT.StartParagraphTok _indent ->
      pure $ AST.ModeDependentCommand $ AST.StartParagraph _indent
    PT.EndParagraphTok ->
      pure $ AST.ModeDependentCommand AST.EndParagraph
    t
      | primTokenHasCategory Code.Space t ->
          pure $ AST.ModeDependentCommand AST.AddSpace
      | primTokenHasCategory Code.BeginGroup t ->
          pure $ AST.ModeIndependentCommand $ AST.ChangeScope Q.Positive HSt.Group.ChangeGroupCharTrigger
      | primTokenHasCategory Code.EndGroup t ->
          pure $ AST.ModeIndependentCommand $ AST.ChangeScope Q.Negative HSt.Group.ChangeGroupCharTrigger
      | primTokenHasCategory Code.MathShift t ->
          pure $ AST.HModeCommand AST.EnterMathMode
    PT.RelaxTok ->
      pure $ AST.ModeIndependentCommand AST.Relax
    PT.IgnoreSpacesTok -> do
      skipOptionalSpaces PT.Expanding
      pure $ AST.ModeIndependentCommand AST.IgnoreSpaces
    PT.PenaltyTok ->
      AST.ModeIndependentCommand . AST.AddPenalty <$> Par.parseInt
    PT.KernTok ->
      AST.ModeIndependentCommand . AST.AddKern <$> Par.parseLength
    PT.MathKernTok ->
      AST.ModeIndependentCommand . AST.AddMathKern <$> Par.parseMathLength
    PT.SetAfterAssignmentTokenTok ->
      AST.ModeIndependentCommand . AST.SetAfterAssignmentToken <$> anyLexInhibited
    PT.AddToAfterGroupTokensTok ->
      AST.ModeIndependentCommand . AST.AddToAfterGroupTokens <$> anyLexInhibited
    PT.CloseInputTok ->
      AST.ModeIndependentCommand . AST.ModifyFileStream . AST.FileStreamModificationCommand AST.FileInput AST.Close <$> Par.parseInt
    PT.DoSpecialTok ->
      AST.ModeIndependentCommand . AST.DoSpecial <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
    PT.RemoveItemTok i ->
      pure $ AST.ModeIndependentCommand $ AST.RemoveItem i
    PT.MessageTok str ->
      AST.ModeIndependentCommand . AST.WriteMessage . AST.MessageWriteCommand str <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
    PT.OpenInputTok ->
      AST.ModeIndependentCommand . AST.ModifyFileStream <$> Par.parseOpenFileStream AST.FileInput
    PT.ImmediateTok -> do
      AST.ModeIndependentCommand
        <$> ( anyPrim
                >>= ( choiceFlap
                        [ fmap AST.ModifyFileStream . Par.headToParseOpenOutput AST.Immediate,
                          fmap AST.ModifyFileStream . Par.headToParseCloseOutput AST.Immediate,
                          fmap AST.WriteToStream . Par.headToParseWriteToStream AST.Immediate
                        ]
                    )
            )
    PT.ModedCommand axis (PT.ShiftedBoxTok direction) -> do
      offsetLength <- Par.parseLength
      let placement = AST.OffsetAlongAxis axis (Box.OffsetInDirection direction offsetLength)
      AST.ModeIndependentCommand . AST.AddBox (Just placement) <$> (anyPrim >>= Par.headToParseBox)
    PT.ChangeScopeCSTok sign ->
      pure $ AST.ModeIndependentCommand $ AST.ChangeScope sign HSt.Group.ChangeGroupCSTrigger
    PT.ControlSpaceTok ->
      pure $ AST.HModeCommand AST.AddControlSpace
    PT.ItalicCorrectionTok ->
      pure $ AST.HModeCommand AST.AddItalicCorrection
    PT.DiscretionaryHyphenTok ->
      pure $ AST.HModeCommand AST.AddDiscretionaryHyphen
    PT.AccentTok -> do
      nr <- Par.parseInt
      assignments <- PC.many Par.parseNonSetBoxAssignment
      chrTok <- PC.optional (anyPrim >>= headToParseCharCodeRef)
      pure $ AST.HModeCommand $ AST.AddAccentedCharacter nr assignments chrTok
    PT.DiscretionaryTextTok ->
      (AST.HModeCommand . AST.AddDiscretionaryText)
        <$> ( AST.DiscretionaryText
                <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
                <*> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
                <*> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
            )
    PT.EndTok ->
      pure $ AST.VModeCommand AST.End
    PT.DumpTok ->
      pure $ AST.VModeCommand AST.Dump
    PT.InsertionTok -> do
      n <- Par.parseInt
      skipFillerExpanding
      pure $ AST.ModeDependentCommand $ AST.AddInsertion n
    PT.AdjustmentTok -> do
      skipFillerExpanding
      pure $ AST.ModeDependentCommand AST.AddAdjustment
    PT.ModedCommand axis PT.AlignedMaterialTok -> do
      boxSpec <- Par.parseBoxSpecification
      pure $ case axis of
        Horizontal ->
          AST.HModeCommand $ AST.AddVAlignedMaterial boxSpec
        Vertical ->
          AST.VModeCommand $ AST.AddHAlignedMaterial boxSpec
    t -> do
      PC.choice
        [ AST.ModeIndependentCommand . AST.Assign <$> Par.headToParseAssignment t,
          AST.ModeIndependentCommand . AST.ModifyFileStream <$> Par.headToParseOpenOutput AST.Deferred t,
          AST.ModeIndependentCommand . AST.ModifyFileStream <$> Par.headToParseCloseOutput AST.Deferred t,
          AST.ModeIndependentCommand . AST.WriteToStream <$> Par.headToParseWriteToStream AST.Deferred t,
          AST.ModeIndependentCommand . AST.AddBox Nothing <$> Par.headToParseBox t,
          AST.HModeCommand . AST.AddCharacter <$> headToParseCharCodeRef t,
          AST.HModeCommand . AST.AddHGlue <$> Par.headToParseModedAddGlue Horizontal t,
          AST.HModeCommand . AST.AddHLeaders <$> Par.headToParseLeadersSpec Horizontal t,
          AST.HModeCommand . AST.AddHRule <$> Par.headToParseModedRule Horizontal t,
          AST.HModeCommand . AST.AddUnwrappedFetchedHBox <$> Par.headToParseFetchedBoxRef Horizontal t,
          AST.VModeCommand . AST.AddVGlue <$> Par.headToParseModedAddGlue Vertical t,
          AST.VModeCommand . AST.AddVLeaders <$> Par.headToParseLeadersSpec Vertical t,
          AST.VModeCommand . AST.AddVRule <$> Par.headToParseModedRule Vertical t,
          AST.VModeCommand . AST.AddUnwrappedFetchedVBox <$> Par.headToParseFetchedBoxRef Vertical t
        ]

headToParseInternalQuantity :: [PrimTokenSource, NonDet, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.InternalQuantity
headToParseInternalQuantity =
  choiceFlap
    [ fmap AST.InternalIntQuantity <$> Par.headToParseInternalInt,
      fmap AST.InternalLengthQuantity <$> Par.headToParseInternalLength,
      fmap AST.InternalGlueQuantity <$> Par.headToParseInternalGlue,
      fmap AST.InternalMathGlueQuantity <$> Par.headToParseInternalMathGlue,
      fmap AST.FontQuantity <$> Par.headToParseFontRef,
      fmap AST.TokenListVariableQuantity <$> Par.headToParseTokenListVariable
    ]

headToParseCharCodeRef :: [PrimTokenSource, NonDet, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.CharCodeRef
headToParseCharCodeRef = \case
  PT.CharCatPair (LT.LexCharCat c Code.Letter) ->
    pure $ AST.CharRef c
  PT.CharCatPair (LT.LexCharCat c Code.Other) ->
    pure $ AST.CharRef c
  PT.ShortDefTargetToken (PT.ShortDefTargetValue PT.CharQuantity i) ->
    pure $ AST.CharTokenRef i
  PT.ControlCharTok ->
    AST.CharCodeNrRef <$> Par.parseCharCodeInt
  t ->
    parseFail $ "headToParseCharCodeRef " <> F.sformat PT.fmtPrimitiveToken t
