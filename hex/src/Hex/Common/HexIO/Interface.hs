{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Hex.Common.HexIO.Interface where

import Formatting qualified as F
import Hex.Common.HexIO.Interface.CharSourceStack
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hexlude

data LexError
  = TerminalEscapeCharacter
  | InvalidCharacter
  | NoMoreLines
  | FileNotFound HexFilePath
  deriving stock (Show, Eq, Generic)

fmtLexError :: Fmt LexError
fmtLexError = F.shown

data FindFilePolicy
  = NoImplicitExtension
  | WithImplicitExtension Text

data InputOrOutput = InputFile | OutputFile
  deriving stock (Show, Generic)

data HexIO :: Effect where
  EndCurrentLine :: HexIO m ()
  InputIsFinished :: HexIO m Bool
  GetInput :: HexIO m CharSourceStack
  PutInput :: CharSourceStack -> HexIO m ()
  InsertLexToken :: LT.LexToken -> HexIO m ()
  InsertLexTokens :: Seq LT.LexToken -> HexIO m ()
  GetNextLexToken :: HexIO m (Maybe LT.LexToken)
  ReadTexFile :: HexFilePath -> HexIO m ()
  OpenStreamFile :: HexFilePath -> Q.FourBitInt -> InputOrOutput -> HexIO m ()

makeEffect ''HexIO

-- If we can resolve lex-tokens, and we have a source of lex-tokens, we can
-- provide a stream of resolved-tokens.
getResolvedToken ::
  ( Error HSt.ResolutionError :> es,
    HexIO :> es,
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
