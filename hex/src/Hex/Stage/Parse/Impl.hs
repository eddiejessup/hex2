{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hex.Stage.Parse.Impl where

import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.Parse (ParseUnexpectedError (..), ParsingError (..))
import Hex.Stage.Expand.Impl.Parse (runParseT)
import Hex.Stage.Expand.Interface qualified as Exp
import Hex.Stage.Lex.Interface qualified as Lex
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Parsers.Command
import Hex.Stage.Parse.Interface
import Hexlude

newtype MonadCommandSourceT m a = MonadCommandSourceT {unMonadCommandSourceT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState st, MonadError e, MonadHexState, Exp.MonadPrimTokenSource, Lex.MonadLexTokenSource)

-- This is quite opaque so to explain:
-- We need (Lex.MonadLexTokenSource m, MonadPrimTokenSource m),
-- because we want 'PrimTokenParse (ParseT m),
-- because that is what we want to run parseCommand in.
-- The instance for 'PrimTokenParse (ParseT m)' requires the above to be true of `m`.
-- So we require that here.
instance
  ( Monad (MonadCommandSourceT m),
    Lex.MonadLexTokenSource (MonadCommandSourceT m),
    Exp.MonadPrimTokenSource (MonadCommandSourceT m),
    MonadError e (MonadCommandSourceT m),
    AsType ParseUnexpectedError e,
    MonadHexState (MonadCommandSourceT m)
  ) =>
  MonadCommandSource (MonadCommandSourceT m)
  where
  getCommand = do
    -- From the perspective of the parser, ie in a MonadPrimTokenSource context,
    -- we need 'end-of-input' to be an error like any other, so we can make a monoid
    -- of the objects to combine parsers together, for 'alternative' behaviour.
    -- But once we're done with parsing, we want to treat end-of-input differently,
    -- by returning a 'Nothing', as we might want to behave differently
    -- instead of just failing in this case.
    runParseT Parsers.Command.parseCommand >>= \case
      Left EndOfInputParsingError -> pure Nothing
      Left (UnexpectedParsingError e) -> throwError $ injectTyped e
      Right cmd -> pure $ Just cmd
