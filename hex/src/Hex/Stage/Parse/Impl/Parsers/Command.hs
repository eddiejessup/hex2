module Hex.Stage.Parse.Impl.Parsers.Command where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as H.Tok
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hex.Common.Parse (MonadPrimTokenParse (..))
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Lex.Interface.Extract qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.BalancedText qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Assignment qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Box qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Command.Stream qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Glue qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.MathLength qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

parseCommand :: MonadPrimTokenParse m => m AST.Command
parseCommand =
  getAnyPrimitiveToken >>= \case
    H.Tok.DebugShowState ->
      pure $ AST.ModeIndependentCommand $ AST.DebugShowState
    H.Tok.ShowTokenTok ->
      AST.ShowToken <$> getAnyLexToken
    H.Tok.ShowBoxTok ->
      AST.ShowBox <$> Par.parseInt
    H.Tok.ShowListsTok ->
      pure AST.ShowLists
    H.Tok.ShowTheInternalQuantityTok ->
      AST.ShowTheInternalQuantity <$> Par.parseHeaded headToParseInternalQuantity
    H.Tok.ShipOutTok ->
      AST.ShipOut <$> Par.parseHeaded Par.headToParseBox
    H.Tok.MarkTok ->
      AST.AddMark <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
    H.Tok.StartParagraphTok _indent ->
      pure $ AST.StartParagraph _indent
    H.Tok.EndParagraphTok ->
      pure AST.EndParagraph
    t
      | Par.primTokenHasCategory Code.Space t ->
          pure AST.AddSpace
      | Par.primTokenHasCategory Code.BeginGroup t ->
          pure $ AST.ModeIndependentCommand $ AST.ChangeScope Q.Positive AST.CharCommandTrigger
      | Par.primTokenHasCategory Code.EndGroup t ->
          pure $ AST.ModeIndependentCommand $ AST.ChangeScope Q.Negative AST.CharCommandTrigger
      | Par.primTokenHasCategory Code.MathShift t ->
          pure $ AST.HModeCommand AST.EnterMathMode
    H.Tok.RelaxTok ->
      pure $ AST.ModeIndependentCommand AST.Relax
    H.Tok.IgnoreSpacesTok -> do
      Par.skipOptionalSpaces
      pure $ AST.ModeIndependentCommand AST.IgnoreSpaces
    H.Tok.PenaltyTok ->
      AST.ModeIndependentCommand . AST.AddPenalty <$> Par.parseInt
    H.Tok.KernTok ->
      AST.ModeIndependentCommand . AST.AddKern <$> Par.parseLength
    H.Tok.MathKernTok ->
      AST.ModeIndependentCommand . AST.AddMathKern <$> Par.parseMathLength
    H.Tok.SetAfterAssignmentTokenTok ->
      AST.ModeIndependentCommand . AST.SetAfterAssignmentToken <$> getAnyLexToken
    H.Tok.AddToAfterGroupTokensTok ->
      AST.ModeIndependentCommand . AST.AddToAfterGroupTokens <$> getAnyLexToken
    H.Tok.CloseInputTok ->
      AST.ModeIndependentCommand . AST.ModifyFileStream . AST.FileStreamModificationCommand AST.FileInput AST.Close <$> Par.parseInt
    H.Tok.DoSpecialTok ->
      AST.ModeIndependentCommand . AST.DoSpecial <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
    H.Tok.RemoveItemTok i ->
      pure $ AST.ModeIndependentCommand $ AST.RemoveItem i
    H.Tok.MessageTok str ->
      AST.ModeIndependentCommand . AST.WriteMessage . AST.MessageWriteCommand str <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
    H.Tok.OpenInputTok ->
      AST.ModeIndependentCommand . AST.ModifyFileStream <$> Par.parseOpenFileStream AST.FileInput
    H.Tok.ImmediateTok -> do
      AST.ModeIndependentCommand
        <$> Par.parseHeaded
          ( Par.choiceFlap
              [ fmap AST.ModifyFileStream . Par.headToParseOpenOutput AST.Immediate,
                fmap AST.ModifyFileStream . Par.headToParseCloseOutput AST.Immediate,
                fmap AST.WriteToStream . Par.headToParseWriteToStream AST.Immediate
              ]
          )
    H.Tok.ModedCommand axis (H.Tok.ShiftedBoxTok direction) -> do
      placement <- AST.ShiftedPlacement axis direction <$> Par.parseLength
      AST.ModeIndependentCommand . AST.AddBox placement <$> Par.parseHeaded Par.headToParseBox
    -- Change scope.
    H.Tok.ChangeScopeCSTok sign ->
      pure $ AST.ModeIndependentCommand $ AST.ChangeScope sign AST.CSCommandTrigger
    H.Tok.ControlSpaceTok ->
      pure $ AST.HModeCommand AST.AddControlSpace
    H.Tok.ItalicCorrectionTok ->
      pure $ AST.HModeCommand AST.AddItalicCorrection
    H.Tok.DiscretionaryHyphenTok ->
      pure $ AST.HModeCommand AST.AddDiscretionaryHyphen
    H.Tok.AccentTok -> do
      nr <- Par.parseInt
      assignments <- PC.many Par.parseNonSetBoxAssignment
      chrTok <- PC.optional (Par.parseHeaded headToParseCharCodeRef)
      pure $ AST.HModeCommand $ AST.AddAccentedCharacter nr assignments chrTok
    H.Tok.DiscretionaryTextTok -> do
      dText <-
        AST.DiscretionaryText
          <$> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
          <*> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
          <*> Par.parseExpandedGeneralText Par.ExpectingBeginGroup
      pure $ AST.HModeCommand $ AST.AddDiscretionaryText dText
    H.Tok.EndTok ->
      pure $ AST.VModeCommand AST.End
    H.Tok.DumpTok ->
      pure $ AST.VModeCommand AST.Dump
    t ->
      PC.choice
        [ AST.ModeIndependentCommand . AST.Assign <$> Par.headToParseAssignment t,
          AST.ModeIndependentCommand . AST.ModifyFileStream <$> Par.headToParseOpenOutput AST.Deferred t,
          AST.ModeIndependentCommand . AST.ModifyFileStream <$> Par.headToParseCloseOutput AST.Deferred t,
          AST.ModeIndependentCommand . AST.WriteToStream <$> Par.headToParseWriteToStream AST.Deferred t,
          AST.ModeIndependentCommand . AST.AddBox AST.NaturalPlacement <$> Par.headToParseBox t,
          AST.HModeCommand . AST.AddCharacter <$> headToParseCharCodeRef t,
          AST.HModeCommand . AST.AddHGlue <$> Par.headToParseModedAddGlue Q.Horizontal t,
          AST.HModeCommand . AST.AddHLeaders <$> Par.headToParseLeadersSpec Q.Horizontal t,
          AST.HModeCommand . AST.AddHRule <$> Par.headToParseModedRule Q.Horizontal t,
          AST.HModeCommand . AST.AddUnwrappedFetchedHBox <$> Par.headToParseFetchedBoxRef Q.Horizontal t,
          AST.VModeCommand . AST.AddVGlue <$> Par.headToParseModedAddGlue Q.Vertical t,
          AST.VModeCommand . AST.AddVLeaders <$> Par.headToParseLeadersSpec Q.Vertical t,
          AST.VModeCommand . AST.AddVRule <$> Par.headToParseModedRule Q.Vertical t,
          AST.VModeCommand . AST.AddUnwrappedFetchedVBox <$> Par.headToParseFetchedBoxRef Q.Vertical t
        ]

headToParseInternalQuantity :: MonadPrimTokenParse m => H.Tok.PrimitiveToken -> m AST.InternalQuantity
headToParseInternalQuantity =
  choiceFlap
    [ fmap AST.InternalIntQuantity <$> Par.headToParseInternalInt,
      fmap AST.InternalLengthQuantity <$> Par.headToParseInternalLength,
      fmap AST.InternalGlueQuantity <$> Par.headToParseInternalGlue,
      -- , fmap AST.InternalMathGlueQuantity  <$> Par.headToParseInternalMathGlue
      fmap AST.FontQuantity <$> Par.headToParseFontRef,
      fmap AST.TokenListVariableQuantity <$> Par.headToParseTokenListVariable
    ]

headToParseCharCodeRef :: MonadPrimTokenParse m => H.Tok.PrimitiveToken -> m AST.CharCodeRef
headToParseCharCodeRef = \case
  T.UnresolvedTok (Lex.CharCatLexToken (Lex.LexCharCat c Code.Letter)) ->
    pure $ AST.CharRef c
  T.UnresolvedTok (Lex.CharCatLexToken (Lex.LexCharCat c Code.Other)) ->
    pure $ AST.CharRef c
  T.IntRefTok T.CharQuantity i ->
    pure $ AST.CharTokenRef i
  T.ControlCharTok ->
    AST.CharCodeNrRef <$> Par.parseCharCodeInt
  _ ->
    empty
