{-# LANGUAGE UndecidableInstances #-}

module Hex.Stage.Parse.Impl where

import Formatting qualified as F
import Hex.Capability.Log.Interface (MonadHexLog)
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexInput.Interface qualified as HIn
import Hex.Common.HexState.Interface (MonadHexState)
import Hex.Common.Parse.Impl (ParseT, fmtParseLog, runParseTMaybe)
import Hex.Common.Parse.Interface (ParseUnexpectedError (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Stage.Expand.Interface qualified as Exp
import Hex.Stage.Parse.Impl.Parsers.Command qualified as Parsers.Command
import Hex.Stage.Parse.Interface
import Hex.Stage.Parse.Interface.AST.Command qualified as Par
import Hexlude

newtype CommandSourceT m a = CommandSourceT {unCommandSourceT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadError e,
      MonadHexState,
      Exp.MonadPrimTokenSource,
      HIn.MonadHexInput,
      MonadHexLog
    )

-- This is quite opaque so to explain:
-- We need (HIn.MonadHexInput m, MonadPrimTokenSource m),
-- because we want 'PrimTokenParse (ParseT m),
-- because that is what we want to run parseCommand in.
-- The instance for 'PrimTokenParse (ParseT m)' requires the above to be true of `m`.
-- So we require that here.
instance
  ( HIn.MonadHexInput (CommandSourceT m),
    Exp.MonadPrimTokenSource (CommandSourceT m),
    MonadError e (CommandSourceT m),
    AsType ParseUnexpectedError e,
    Log.MonadHexLog (CommandSourceT m),
    Monad m
  ) =>
  MonadCommandSource (CommandSourceT m)
  where
  getCommand = getCommandImpl

getCommandImpl ::
  ( Monad m,
    Par.MonadPrimTokenParse (ParseT m),
    MonadError e m,
    AsType ParseUnexpectedError e,
    Log.MonadHexLog m
  ) =>
  m (Maybe Par.Command)
getCommandImpl = do
  (mayCmd, pLog) <- runParseTMaybe Parsers.Command.parseCommand
  Log.infoLog $ F.sformat ("Parsed command: " |%| fmtParseLog) pLog
  pure mayCmd
