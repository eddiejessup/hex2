module Hex.Parse.Parsers.Command where

-- import Hex.Parse.MonadParse.Interface

import Control.Monad.Combinators qualified as PC
import Control.Monad.Trans (MonadTrans (..))
import Hex.Codes qualified as H.Codes
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.AST qualified as H.AST
import Hex.Parse.MonadTokenSource.Interface qualified as H.Par.TokSrc
import Hex.Symbol.Resolve qualified as H.Sym.Res
import Hex.Symbol.Tokens qualified as H.Tok
import Hexlude
import Hex.Parse.MonadPrimTokenSource.Interface

parseCommand :: MonadPrimTokenSource m => m H.AST.Command
parseCommand = undefined
--   lift fetchPrimitiveToken >>= \case
--     -- H.Tok.ShowTokenTok ->
--     --     H.AST.ShowToken <$> parseLexToken
--     -- H.Tok.ShowBoxTok ->
--     --     H.AST.ShowBox <$> parseInt
--     -- H.Tok.ShowListsTok ->
--     --     pure ShowLists
--     -- H.Tok.ShowTheInternalQuantityTok ->
--     --     H.AST.ShowTheInternalQuantity <$> parseInternalQuantity
--     -- H.Tok.ShipOutTok ->
--     --     H.AST.ShipOut <$> parseHeaded headToParseBox
--     -- H.Tok.MarkTok ->
--     --     H.AST.AddMark <$> parseGeneralText
--     H.Tok.StartParagraphTok _indent ->
--       pure $ H.AST.StartParagraph _indent
--     H.Tok.EndParagraphTok ->
--       pure H.AST.EndParagraph
--     t
--       | isSpace t ->
--         pure H.AST.AddSpace
--       | primTokHasCategory H.Codes.BeginGroup t ->
--         pure $ H.AST.ModeIndependentCommand $ H.AST.ChangeScope H.Tok.Positive H.AST.CharCommandTrigger
--       | primTokHasCategory H.Codes.EndGroup t ->
--         pure $ H.AST.ModeIndependentCommand $ H.AST.ChangeScope H.Tok.Negative H.AST.CharCommandTrigger
--       | primTokHasCategory H.Codes.MathShift t ->
--         pure $ H.AST.HModeCommand H.AST.EnterMathMode
--     H.Tok.RelaxTok ->
--       pure $ H.AST.ModeIndependentCommand H.AST.Relax
--     H.Tok.IgnoreSpacesTok -> do
--       skipOptionalSpaces
--       pure $ H.AST.ModeIndependentCommand H.AST.IgnoreSpaces
--     H.Tok.PenaltyTok ->
--       H.AST.ModeIndependentCommand . H.AST.AddPenalty <$> parseInt

-- H.Tok.KernTok ->
--   H.AST.AddKern <$> parseLength
-- H.Tok.MathKernTok ->
--   H.AST.AddMathKern <$> parseMathLength
-- H.Tok.SetAfterAssignmentTokenTok ->
--   H.AST.SetAfterAssignmentToken <$> parseLexToken
-- H.Tok.AddToAfterGroupTokensTok ->
--   H.AST.AddToAfterGroupTokens <$> parseLexToken
-- H.Tok.CloseInputTok ->
--   H.AST.ModifyFileStream H.AST.FileInput H.AST.Close <$> parseInt
-- H.Tok.DoSpecialTok ->
--   H.AST.DoSpecial <$> parseExpandedGeneralText
-- H.Tok.RemoveItemTok i ->
--   pure (H.AST.RemoveItem i)
-- H.Tok.MessageTok str ->
--   H.AST.Message str <$> parseExpandedGeneralText
-- H.Tok.OpenInputTok ->
--   parseModifyFileStream FileInput
-- H.Tok.ImmediateTok -> do
--   t2 <- fetchPrimitiveToken
--   fooChoice t2
--     [ headToParseOpenOutput Immediate t2
--     , headToParseCloseOutput Immediate t2
--     , headToParseWriteToStream Immediate t2
--     ]
-- H.Tok.ModedCommand axis (H.Tok.ShiftedBoxTok direction) -> do
--   placement <- H.AST.ShiftedPlacement axis direction <$> parseLength
--   H.AST.AddBox placement <$> parseHeaded headToParseBox
-- -- Change scope.
-- H.Tok.ChangeScopeCSTok sign ->
--   pure $ H.AST.ChangeScope sign H.AST.CSCommandTrigger

--     H.Tok.ControlSpaceTok ->
--         pure AddControlSpace
--     H.Tok.ItalicCorrectionTok ->
--         pure AddItalicCorrection
--     H.Tok.DiscretionaryHyphenTok ->
--         pure AddDiscretionaryHyphen
--     H.Tok.AccentTok ->
--         AddAccentedCharacter
--             <$> parseInt
--             <*> fooMany parseNonSetBoxAssignment
--             <*> fooOptional (parseHeaded headToParseCharCodeRef)
--     H.Tok.DiscretionaryTextTok ->
--         AddDiscretionaryText
--             <$> parseGeneralText
--             <*> parseGeneralText
--             <*> parseGeneralText
--     H.Tok.EndTok ->
--         pure End
--     H.Tok.DumpTok ->
--         pure Dump

--     _ ->
--       fooChoice
--         [ fmap H.AST.Assign <$> headToParseAssignment t
--         , headToParseOpenOutput H.AST.Deferred t
--         , headToParseCloseOutput H.AST.Deferred t
--         , headToParseWriteToStream H.AST.Deferred t
--         , fmap (H.AST.AddBox H.AST.NaturalPlacement) <$> headToParseBox t

--         , fmap H.AST.AddCharacter <$> headToParseCharCodeRef
--         , fmap H.AST.AddHGlue <$> headToParseModedGlue Horizontal
--         , fmap H.AST.AddHLeaders <$> headToParseLeadersSpec Horizontal
--         , fmap H.AST.AddHRule <$> headToParseModedRule Horizontal
--         , fmap H.AST.AddUnwrappedFetchedHBox <$> headToParseFetchedBoxRef Horizontal

--         , fmap H.AST.AddVGlue <$> headToParseModedGlue Vertical
--         , fmap H.AST.AddVLeaders <$> headToParseLeadersSpec Vertical
--         , fmap H.AST.AddVRule <$> headToParseModedRule Vertical
--         , fmap H.AST.AddUnwrappedFetchedVBox <$> headToParseFetchedBoxRef Vertical
--         ]
