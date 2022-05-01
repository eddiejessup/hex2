{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Parse.Impl where

import Hex.Common.Parse (ParsingError (..), ParseUnexpectedError(..))
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Parsers.Command
import Hex.Stage.Parse.Interface
import Hexlude
import Hex.Stage.Expand.Impl.Parse (runParseT)
import qualified Hex.Stage.Lex.Interface as Lex
import qualified Hex.Stage.Expand.Interface as Exp

-- This is quite opaque so to explain:
-- We need (Lex.MonadLexTokenSource m, MonadPrimTokenSource m),
-- because we want 'PrimTokenParse (ParseT m),
-- because that is what we want to run parseCommand in.
-- The instance for 'PrimTokenParse (ParseT m)' requires the above to be true of `m`.
-- So we require that here.
instance (Lex.MonadLexTokenSource m, Exp.MonadPrimTokenSource m, MonadError e m, AsType ParseUnexpectedError e) => MonadCommandSource m where
  getCommand = do
    -- From the perspective of the parser, ie in a MonadPrimTokenSource context,
    -- we need 'end-of-input' to be an error like any other, so we can make a monoid
    -- of the objects to combine parsers together, for 'alternative' behaviour.
    -- But once we're done with parsing, we want to treat end-of-input differently,
    -- by returning a 'Nothing', as we might want to behave differently
    -- instead of just failing in this case.
    runParseT Parsers.Command.parseCommand >>= \case
      Left ParseEndOfInput -> pure Nothing
      Left (ParseUnexpectedError e) -> throwError $ injectTyped e
      Right cmd -> pure $ Just cmd

-- getSource = Expand.getSource
-- putSource = Expand.putSource
-- insertLexTokenToSource = Expand.insertLexTokenToSource
-- insertLexTokensToSource = Expand.insertLexTokensToSource
