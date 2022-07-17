module Hex.Common.HexInput.Interface where

import Formatting qualified as F
import Hex.Common.HexInput.Interface.CharSourceStack
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.Token.Lexed qualified as LT
import Hex.Common.Token.Resolved qualified as RT
import Hexlude

data LexError
  = TerminalEscapeCharacter
  | InvalidCharacter
  | NoMoreLines
  | InputFileNotFound HexFilePath
  deriving stock (Show, Eq, Generic)

fmtLexError :: Fmt LexError
fmtLexError = F.shown

class Monad m => MonadHexInput m where
  endCurrentLine :: m ()

  inputIsFinished :: m Bool

  getInput :: m CharSourceStack

  putInput :: CharSourceStack -> m ()

  insertLexToken :: LT.LexToken -> m ()

  insertLexTokens :: Seq LT.LexToken -> m ()

  getNextLexToken :: m (Maybe LT.LexToken)

  openInputFile :: HexFilePath -> m ()

-- If we can resolve lex-tokens, and we have a source of lex-tokens, we can
-- provide a stream of resolved-tokens.
getMayResolvedToken :: (HSt.MonadHexState m, MonadHexInput m) => m (Maybe (LT.LexToken, Either HSt.ResolutionError RT.ResolvedToken))
getMayResolvedToken =
  -- Get a lex token.
  getNextLexToken >>= \case
    -- If no lex token, return nothing.
    Nothing -> pure Nothing
    -- If there is a lex token, try to resolve it.
    Just lt -> do
      errOrRT <- HSt.resolveLexToken lt
      pure $ Just (lt, errOrRT)
