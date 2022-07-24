{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Hex.Stage.Read.Interface where

import Formatting qualified as F
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hex.Stage.Read.Interface.CharSourceStack
import Hexlude

data LexError
  = TerminalEscapeCharacter
  | InvalidCharacter
  | NoMoreLines
  | InputFileNotFound HexFilePath
  deriving stock (Show, Eq, Generic)

fmtLexError :: Fmt LexError
fmtLexError = F.shown

data HexInput :: Effect where
  EndCurrentLine :: HexInput m ()
  InputIsFinished :: HexInput m Bool
  GetInput :: HexInput m CharSourceStack
  PutInput :: CharSourceStack -> HexInput m ()
  InsertLexToken :: LT.LexToken -> HexInput m ()
  InsertLexTokens :: Seq LT.LexToken -> HexInput m ()
  GetNextLexToken :: HexInput m (Maybe LT.LexToken)
  OpenInputFile :: HexFilePath -> HexInput m ()

makeEffect ''HexInput

-- If we can resolve lex-tokens, and we have a source of lex-tokens, we can
-- provide a stream of resolved-tokens.
getResolvedToken ::
  ( Error HSt.ResolutionError :> es,
    HexInput :> es,
    HSt.EHexState :> es
  ) =>
  Eff es (Maybe (LT.LexToken, RT.ResolvedToken))
getResolvedToken =
  getNextLexToken >>= \case
    -- If nothing left in the input, return nothing.
    Nothing -> pure Nothing
    -- If we get a token, we only care about the resolved version.
    -- Check if resolution succeeded.
    Just lt -> do
      rt <- HSt.resolveLexToken lt
      pure $ Just (lt, rt)
