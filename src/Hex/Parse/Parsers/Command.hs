module Hex.Parse.Parsers.Command where

import Control.Monad.Combinators qualified as PC
import Hex.Codes qualified as H.C
import Hex.Lex.Types qualified as H.Lex
import Hex.Syntax.Command qualified as H.Syn
import Hex.Syntax.Quantity qualified as H.Syn
import Hex.Syntax.Common qualified as H.Syn
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.BalancedText qualified as Par
import Hex.Parse.Parsers.Combinators
import Hex.Parse.Parsers.Combinators qualified as Par
import Hex.Parse.Parsers.Command.Assignment qualified as Par
import Hex.Parse.Parsers.Command.Box qualified as Par
import Hex.Parse.Parsers.Command.Stream qualified as Par
import Hex.Parse.Parsers.Quantity.Glue qualified as Par
import Hex.Parse.Parsers.Quantity.Length qualified as Par
import Hex.Parse.Parsers.Quantity.MathLength qualified as Par
import Hex.Parse.Parsers.Quantity.Number qualified as Par
import Hex.Quantity qualified as H.Q
import Hex.Symbol.Token.Primitive qualified as H.Tok
import Hex.Symbol.Token.Primitive qualified as T
import Hexlude

parseCommand :: MonadPrimTokenSource m => m (H.Syn.Command 'H.Syn.Parsed)
parseCommand =
  fetchPT >>= \case
    H.Tok.ShowTokenTok ->
      H.Syn.ShowToken <$> Par.fetchInhibitedLexToken
    H.Tok.ShowBoxTok ->
      H.Syn.ShowBox <$> Par.parseInt
    H.Tok.ShowListsTok ->
      pure H.Syn.ShowLists
    H.Tok.ShowTheInternalQuantityTok ->
      H.Syn.ShowTheInternalQuantity <$> Par.parseHeaded headToParseInternalQuantity
    H.Tok.ShipOutTok ->
      H.Syn.ShipOut <$> Par.parseHeaded Par.headToParseBox
    H.Tok.MarkTok ->
      H.Syn.AddMark <$> Par.parseExpandedGeneralText
    H.Tok.StartParagraphTok _indent ->
      pure $ H.Syn.StartParagraph _indent
    H.Tok.EndParagraphTok ->
      pure H.Syn.EndParagraph
    t
      | Par.primTokHasCategory H.C.Space t ->
        pure H.Syn.AddSpace
      | Par.primTokHasCategory H.C.BeginGroup t ->
        pure $ H.Syn.ModeIndependentCommand $ H.Syn.ChangeScope H.Q.Positive H.Syn.CharCommandTrigger
      | Par.primTokHasCategory H.C.EndGroup t ->
        pure $ H.Syn.ModeIndependentCommand $ H.Syn.ChangeScope H.Q.Negative H.Syn.CharCommandTrigger
      | Par.primTokHasCategory H.C.MathShift t ->
        pure $ H.Syn.HModeCommand H.Syn.EnterMathMode
    H.Tok.RelaxTok ->
      pure $ H.Syn.ModeIndependentCommand H.Syn.Relax
    H.Tok.IgnoreSpacesTok -> do
      Par.skipOptionalSpaces
      pure $ H.Syn.ModeIndependentCommand H.Syn.IgnoreSpaces
    H.Tok.PenaltyTok ->
      H.Syn.ModeIndependentCommand . H.Syn.AddPenalty <$> Par.parseInt
    H.Tok.KernTok ->
      H.Syn.ModeIndependentCommand . H.Syn.AddKern <$> Par.parseLength
    H.Tok.MathKernTok ->
      H.Syn.ModeIndependentCommand . H.Syn.AddMathKern <$> Par.parseMathLength
    H.Tok.SetAfterAssignmentTokenTok ->
      H.Syn.ModeIndependentCommand . H.Syn.SetAfterAssignmentToken <$> Par.fetchInhibitedLexToken
    H.Tok.AddToAfterGroupTokensTok ->
      H.Syn.ModeIndependentCommand . H.Syn.AddToAfterGroupTokens <$> Par.fetchInhibitedLexToken
    H.Tok.CloseInputTok ->
      H.Syn.ModeIndependentCommand . H.Syn.ModifyFileStream . H.Syn.FileStreamModificationCommand H.Syn.FileInput H.Syn.Close <$> Par.parseInt
    H.Tok.DoSpecialTok ->
      H.Syn.ModeIndependentCommand . H.Syn.DoSpecial <$> Par.parseExpandedGeneralText
    H.Tok.RemoveItemTok i ->
      pure $ H.Syn.ModeIndependentCommand $ H.Syn.RemoveItem i
    H.Tok.MessageTok str ->
      H.Syn.ModeIndependentCommand . H.Syn.WriteMessage . H.Syn.MessageWriteCommand str <$> Par.parseExpandedGeneralText
    H.Tok.OpenInputTok ->
      H.Syn.ModeIndependentCommand . H.Syn.ModifyFileStream <$> Par.parseOpenFileStream H.Syn.FileInput
    H.Tok.ImmediateTok -> do
      H.Syn.ModeIndependentCommand
        <$> Par.parseHeaded
          ( Par.choiceFlap
              [ fmap H.Syn.ModifyFileStream . Par.headToParseOpenOutput H.Syn.Immediate,
                fmap H.Syn.ModifyFileStream . Par.headToParseCloseOutput H.Syn.Immediate,
                fmap H.Syn.WriteToStream . Par.headToParseWriteToStream H.Syn.Immediate
              ]
          )
    H.Tok.ModedCommand axis (H.Tok.ShiftedBoxTok direction) -> do
      placement <- H.Syn.ShiftedPlacement axis direction <$> Par.parseLength
      H.Syn.ModeIndependentCommand . H.Syn.AddBox placement <$> Par.parseHeaded Par.headToParseBox
    -- Change scope.
    H.Tok.ChangeScopeCSTok sign ->
      pure $ H.Syn.ModeIndependentCommand $ H.Syn.ChangeScope sign H.Syn.CSCommandTrigger
    H.Tok.ControlSpaceTok ->
      pure $ H.Syn.HModeCommand H.Syn.AddControlSpace
    H.Tok.ItalicCorrectionTok ->
      pure $ H.Syn.HModeCommand H.Syn.AddItalicCorrection
    H.Tok.DiscretionaryHyphenTok ->
      pure $ H.Syn.HModeCommand H.Syn.AddDiscretionaryHyphen
    H.Tok.AccentTok -> do
      nr <- Par.parseInt
      assignments <- PC.many Par.parseNonSetBoxAssignment
      chrTok <- PC.optional (Par.parseHeaded headToParseCharCodeRef)
      pure $ H.Syn.HModeCommand $ H.Syn.AddAccentedCharacter nr assignments chrTok
    H.Tok.DiscretionaryTextTok -> do
      dText <-
        H.Syn.DiscretionaryText
          <$> Par.parseExpandedGeneralText
          <*> Par.parseExpandedGeneralText
          <*> Par.parseExpandedGeneralText
      pure $ H.Syn.HModeCommand $ H.Syn.AddDiscretionaryText dText
    H.Tok.EndTok ->
      pure $ H.Syn.VModeCommand H.Syn.End
    H.Tok.DumpTok ->
      pure $ H.Syn.VModeCommand H.Syn.Dump
    t ->
      PC.choice
        [ H.Syn.ModeIndependentCommand . H.Syn.Assign <$> Par.headToParseAssignment t,
          H.Syn.ModeIndependentCommand . H.Syn.ModifyFileStream <$> Par.headToParseOpenOutput H.Syn.Deferred t,
          H.Syn.ModeIndependentCommand . H.Syn.ModifyFileStream <$> Par.headToParseCloseOutput H.Syn.Deferred t,
          H.Syn.ModeIndependentCommand . H.Syn.WriteToStream <$> Par.headToParseWriteToStream H.Syn.Deferred t,
          H.Syn.ModeIndependentCommand . H.Syn.AddBox H.Syn.NaturalPlacement <$> Par.headToParseBox t,
          H.Syn.HModeCommand . H.Syn.AddCharacter <$> headToParseCharCodeRef t,
          H.Syn.HModeCommand . H.Syn.AddHGlue <$> Par.headToParseModedAddGlue H.Q.Horizontal t,
          H.Syn.HModeCommand . H.Syn.AddHLeaders <$> Par.headToParseLeadersSpec H.Q.Horizontal t,
          H.Syn.HModeCommand . H.Syn.AddHRule <$> Par.headToParseModedRule H.Q.Horizontal t,
          H.Syn.HModeCommand . H.Syn.AddUnwrappedFetchedHBox <$> Par.headToParseFetchedBoxRef H.Q.Horizontal t,
          H.Syn.VModeCommand . H.Syn.AddVGlue <$> Par.headToParseModedAddGlue H.Q.Vertical t,
          H.Syn.VModeCommand . H.Syn.AddVLeaders <$> Par.headToParseLeadersSpec H.Q.Vertical t,
          H.Syn.VModeCommand . H.Syn.AddVRule <$> Par.headToParseModedRule H.Q.Vertical t,
          H.Syn.VModeCommand . H.Syn.AddUnwrappedFetchedVBox <$> Par.headToParseFetchedBoxRef H.Q.Vertical t
        ]

headToParseInternalQuantity :: MonadPrimTokenSource m => H.Tok.PrimitiveToken -> m (H.Syn.InternalQuantity 'H.Syn.Parsed)
headToParseInternalQuantity =
  choiceFlap
    [ fmap H.Syn.InternalIntQuantity <$> Par.headToParseInternalInt,
      fmap H.Syn.InternalLengthQuantity <$> Par.headToParseInternalLength,
      fmap H.Syn.InternalGlueQuantity <$> Par.headToParseInternalGlue,
      -- , fmap H.Syn.InternalMathGlueQuantity  <$> Par.headToParseInternalMathGlue
      fmap H.Syn.FontQuantity <$> Par.headToParseFontRef,
      fmap H.Syn.TokenListVariableQuantity <$> Par.headToParseTokenListVariable
    ]

headToParseCharCodeRef :: MonadPrimTokenSource m => H.Tok.PrimitiveToken -> m (H.Syn.CharCodeRef 'H.Syn.Parsed)
headToParseCharCodeRef = \case
  T.UnresolvedTok (H.Lex.CharCatLexToken (H.Lex.LexCharCat c H.C.Letter)) ->
    pure $ H.Syn.CharRef c
  T.UnresolvedTok (H.Lex.CharCatLexToken (H.Lex.LexCharCat c H.C.Other)) ->
    pure $ H.Syn.CharRef c
  T.IntRefTok T.CharQuantity i ->
    pure $ H.Syn.CharTokenRef i
  T.ControlCharTok ->
    H.Syn.CharCodeNrRef <$> Par.parseInt
  _ ->
    empty
