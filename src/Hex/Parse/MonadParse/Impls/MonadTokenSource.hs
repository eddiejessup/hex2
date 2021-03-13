{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Parse.MonadParse.Impls.MonadTokenSource where

import Hex.Codes qualified as H.Codes
import Hex.Lex.Types qualified as H.Lex
import Hex.Parse.AST qualified as H.Par.AST
import Hex.Parse.MonadParse.Interface
import Hex.Parse.MonadTokenSource.Interface qualified as H.Par.TokSrc
import Hex.Symbol.Resolve qualified as H.Sym.Res
import Hex.Symbol.Tokens qualified as H.Sym.Tok
import Hexlude

expandSyntaxCommand ::
  ( MonadParse m
  ) =>
  H.Sym.Tok.SyntaxCommandHeadToken ->
  m (Seq H.Lex.LexToken)
expandSyntaxCommand = \case

-- MacroTok m -> do
--   args <- parseMacroArgs m
--   expandMacro m args
-- ConditionTok ct -> do
--   expandConditionToken ct
--   pure mempty
-- NumberTok ->
--   panic "Not implemented: syntax command NumberTok"
-- RomanNumeralTok ->
--   panic "Not implemented: syntax command RomanNumeralTok"
-- StringTok -> do
--   conf <- use $ typed @Conf.Config
--   let escapeChar = (Conf.IntParamVal . Conf.lookupIntParameter EscapeChar) conf
--   expandString escapeChar <$> parseLexToken
-- JobNameTok ->
--   panic "Not implemented: syntax command JobNameTok"
-- FontNameTok ->
--   panic "Not implemented: syntax command FontNameTok"
-- MeaningTok ->
--   panic "Not implemented: syntax command MeaningTok"
-- CSNameTok -> do
--   a <- parseCSNameArgs
--   singleton <$> expandCSName a
-- ExpandAfterTok -> do
--   argLT <- takeLexToken
--   (_, postArgLTs) <- takeAndExpandResolvedToken
--   -- Prepend the unexpanded token.
--   pure (argLT <| postArgLTs)
-- NoExpandTok ->
--   panic "Not implemented: syntax command NoExpandTok"
-- MarkRegisterTok _ ->
--   panic "Not implemented: syntax command MarkRegisterTok"
-- -- \input ⟨file name⟩:
-- -- - Expand to no tokens
-- -- - Prepare to read from the specified file before looking at any more
-- --   tokens from the current source.
-- InputTok -> do
--   TeXFilePath texPath <- parseFileName
--   inputPath texPath
--   pure mempty
-- EndInputTok ->
--   panic "Not implemented: syntax command EndInputTok"
-- TheTok -> do
--   intQuant <- parseInternalQuantity
--   fmap charCodeAsMadeToken <$> texEvaluate intQuant
-- ChangeCaseTok direction -> do
--   conf <- use $ typed @Conf.Config
--   expandChangeCase
--     (\c -> Conf.lookupChangeCaseCode direction c conf)
--     <$> parseGeneralText

data ParseError
  = UnexpectedEndOfInput
  deriving stock (Generic, Show)

fetchPrimitiveToken :: (H.Par.TokSrc.MonadTokenSource m, MonadError e m, AsType ParseError e) => m H.Sym.Tok.PrimitiveToken
fetchPrimitiveToken = do
  H.Par.TokSrc.getLexToken >>= \case
    Nothing -> throwError $ injectTyped UnexpectedEndOfInput
    Just lt ->
      H.Par.TokSrc.resolveLexToken H.Sym.Res.Resolving lt >>= \case
        Left H.Par.TokSrc.ResolutionError ->
          panic "Bad resolve"
        Right rt ->
          case rt of
            H.Sym.Tok.PrimitiveToken pt ->
              pure pt
            H.Sym.Tok.SyntaxCommandHeadToken headTok -> do
              lts <- expandSyntaxCommand headTok
              H.Par.TokSrc.insertLexTokensToSource lts
              fetchPrimitiveToken

instance (Monad m, H.Par.TokSrc.MonadTokenSource m, MonadError e m, AsType ParseError e) => MonadParse m where
  parseCommand =
    fetchPrimitiveToken >>= \case
      -- H.Sym.Tok.ShowTokenTok ->
      --   ShowToken <$> parseLexToken
      -- H.Sym.Tok.ShowBoxTok ->
      --   ShowBox <$> parseHexInt
      -- H.Sym.Tok.ShowListsTok ->
      --   pure ShowLists
      -- H.Sym.Tok.ShowTheInternalQuantityTok ->
      --   ShowTheInternalQuantity <$> parseInternalQuantity
      -- H.Sym.Tok.ShipOutTok ->
      --   ShipOut <$> parseHeaded headToParseBox
      -- H.Sym.Tok.MarkTok ->
      --   AddMark <$> parseGeneralText
      H.Sym.Tok.StartParagraphTok _indent ->
        pure $ H.Par.AST.StartParagraph _indent
      H.Sym.Tok.EndParagraphTok ->
        pure H.Par.AST.EndParagraph
      t
        | primTokHasCategory H.Codes.Space t ->
          pure H.Par.AST.AddSpace
      -- , parseInsert
      -- , parseVAdjust
      -- , parseAlign mode

      -- parseModeIndependentCommand:
      H.Sym.Tok.RelaxTok ->
        pure $ H.Par.AST.ModeIndependentCommand H.Par.AST.Relax
      H.Sym.Tok.IgnoreSpacesTok -> do
        skipOptionalSpaces
        pure $ H.Par.AST.ModeIndependentCommand H.Par.AST.IgnoreSpaces
      H.Sym.Tok.PenaltyTok ->
        H.Par.AST.ModeIndependentCommand . H.Par.AST.AddPenalty <$> parseHexInt
      H.Sym.Tok.KernTok ->
        H.Par.AST.ModeIndependentCommand . H.Par.AST.AddKern <$> parseLength
      H.Sym.Tok.MathKernTok ->
        H.Par.AST.ModeIndependentCommand . H.Par.AST.AddMathKern <$> parseMathLength
      H.Sym.Tok.SetAfterAssignmentTokenTok ->
        H.Par.AST.ModeIndependentCommand . H.Par.AST.SetAfterAssignmentToken <$> parseLexToken
      H.Sym.Tok.AddToAfterGroupTokensTok ->
        H.Par.AST.ModeIndependentCommand . H.Par.AST.AddToAfterGroupTokens <$> parseLexToken
      H.Sym.Tok.CloseInputTok ->
        H.Par.AST.ModeIndependentCommand . H.Par.AST.ModifyFileStream H.Par.AST.FileInput H.Par.AST.Close <$> parseHexInt
      H.Sym.Tok.DoSpecialTok ->
        H.Par.AST.ModeIndependentCommand . H.Par.AST.DoSpecial <$> parseExpandedGeneralText
      H.Sym.Tok.RemoveItemTok i ->
        pure $ H.Par.AST.ModeIndependentCommand (H.Par.AST.RemoveItem i)
      H.Sym.Tok.MessageTok str ->
        H.Par.AST.ModeIndependentCommand . H.Par.AST.Message str <$> parseExpandedGeneralText
      H.Sym.Tok.OpenInputTok ->
        parseModifyFileStream H.Par.AST.FileInput
      H.Sym.Tok.ImmediateTok ->
        undefined
      -- PC.choice
      --   [ parseHeaded (headToParseOpenOutput Immediate)
      --   , parseHeaded (headToParseCloseOutput Immediate)
      --   , parseHeaded (headToParseWriteToStream Immediate)
      --   ]
      H.Sym.Tok.ModedCommand axis (H.Sym.Tok.ShiftedBoxTok direction) -> do
        placement <- H.Par.AST.ShiftedPlacement axis direction <$> parseLength
        H.Par.AST.ModeIndependentCommand . H.Par.AST.AddBox placement <$> parseHeaded headToParseBox
      -- Change scope.
      H.Sym.Tok.ChangeScopeCSTok sign ->
        pure $ H.Par.AST.ModeIndependentCommand $ H.Par.AST.ChangeScope sign H.Par.AST.CSCommandTrigger
      t
        | primTokHasCategory H.Codes.BeginGroup t ->
          pure $ H.Par.AST.ModeIndependentCommand $ H.Par.AST.ChangeScope H.Sym.Tok.Positive H.Par.AST.CharCommandTrigger
        | primTokHasCategory H.Codes.EndGroup t ->
          pure $ H.Par.AST.ModeIndependentCommand $ H.Par.AST.ChangeScope H.Sym.Tok.Negative H.Par.AST.CharCommandTrigger
      -- , fmap Assign <$> headToParseAssignment
      -- , headToParseOpenOutput Deferred
      -- , headToParseCloseOutput Deferred
      -- , headToParseWriteToStream Deferred
      -- , fmap (AddBox NaturalPlacement) <$> headToParseBox

      -- parseHModeCommand:
      H.Sym.Tok.ControlSpaceTok ->
        pure $ H.Par.AST.HModeCommand H.Par.AST.AddControlSpace
      H.Sym.Tok.ItalicCorrectionTok ->
        pure $ H.Par.AST.HModeCommand H.Par.AST.AddItalicCorrection
      H.Sym.Tok.DiscretionaryHyphenTok ->
        pure $ H.Par.AST.HModeCommand H.Par.AST.AddDiscretionaryHyphen
      t
        | primTokHasCategory H.Codes.MathShift t ->
          pure $ H.Par.AST.HModeCommand H.Par.AST.EnterMathMode
      H.Sym.Tok.AccentTok ->
        undefined
      -- H.Par.AST.AddAccentedCharacter
      --   <$> parseHexInt
      --   <*> PC.many parseNonSetBoxAssignment
      --   <*> PC.optional (parseHeaded headToParseCharCodeRef)
      H.Sym.Tok.DiscretionaryTextTok ->
        undefined
      -- H.Par.AST.AddDiscretionaryText
      --   <$> parseGeneralText
      --   <*> parseGeneralText
      --   <*> parseGeneralText
      -- , fmap AddCharacter <$> headToParseCharCodeRef
      -- Add character.
      H.Sym.Tok.UnresolvedTok (H.Lex.CharCatLexToken (H.Lex.LexCharCat c H.Codes.Letter)) ->
        pure $ H.Par.AST.HModeCommand $ H.Par.AST.AddCharacter $ H.Par.AST.CharRef c
      H.Sym.Tok.UnresolvedTok (H.Lex.CharCatLexToken (H.Lex.LexCharCat c H.Codes.Other)) ->
        pure $ H.Par.AST.HModeCommand $ H.Par.AST.AddCharacter $ H.Par.AST.CharRef c
      H.Sym.Tok.IntRefTok H.Sym.Tok.CharQuantity i ->
        pure $ H.Par.AST.HModeCommand $ H.Par.AST.AddCharacter $ H.Par.AST.CharTokenRef i
      H.Sym.Tok.ControlCharTok ->
        H.Par.AST.HModeCommand . H.Par.AST.AddCharacter . H.Par.AST.CharCodeNrRef <$> parseHexInt
      -- , fmap AddHGlue <$> headToParseModedGlue Horizontal
      -- , fmap AddHLeaders <$> headToParseLeadersSpec Horizontal
      -- , fmap AddHRule <$> headToParseModedRule Horizontal
      -- , fmap AddUnwrappedFetchedHBox <$> headToParseFetchedBoxRef Horizontal

      -- , parseVModeCommand:
      H.Sym.Tok.EndTok ->
        pure $ H.Par.AST.VModeCommand H.Par.AST.End
      H.Sym.Tok.DumpTok ->
        pure $ H.Par.AST.VModeCommand H.Par.AST.Dump
      -- , fmap AddVGlue <$> headToParseModedGlue Vertical
      -- , fmap AddVLeaders <$> headToParseLeadersSpec Vertical
      -- , fmap AddVRule <$> headToParseModedRule Vertical
      -- , fmap AddUnwrappedFetchedVBox <$> headToParseFetchedBoxRef Vertical
      t ->
        panic $ "Unexpected token: " <> show t
    where
      skipOptionalSpaces = undefined
      parseHexInt = undefined
      parseLength = undefined
      parseMathLength = undefined
      parseLexToken = undefined
      parseExpandedGeneralText = undefined
      parseModifyFileStream = undefined
      parseHeaded = undefined
      headToParseBox = undefined

  insertLexTokensToStream = H.Par.TokSrc.insertLexTokensToSource

  insertLexTokenToStream = H.Par.TokSrc.insertLexTokenToSource

  getStream = H.Par.TokSrc.getSource

  putStream = H.Par.TokSrc.putSource

primTokHasCategory :: H.Codes.CoreCatCode -> H.Sym.Tok.PrimitiveToken -> Bool
primTokHasCategory a (H.Sym.Tok.UnresolvedTok lt) = lexTokHasCategory a lt
primTokHasCategory _ _ = False

lexTokHasCategory :: H.Codes.CoreCatCode -> H.Lex.LexToken -> Bool
lexTokHasCategory a (H.Lex.CharCatLexToken cc) = ccHasCategory a cc
lexTokHasCategory _ _ = False

ccHasCategory :: H.Codes.CoreCatCode -> H.Lex.LexCharCat -> Bool
ccHasCategory a H.Lex.LexCharCat {H.Lex.lexCCCat = b} = a == b
