{-# OPTIONS_GHC -Wno-missing-methods #-}

module Common where

import Data.ByteString qualified as BS
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexEnv.Interface
import Hex.Common.HexInput.Impl
import Hex.Common.HexInput.Interface
import Hex.Common.HexState.Impl
import Hex.Common.HexState.Interface
import Hex.Run.App
import Hex.Run.App qualified as App
import Hexlude

newtype TestApp a = TestApp {unTestApp :: StateT AppState (ExceptT AppError IO) a}
  deriving stock (Generic)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError AppError,
      MonadState AppState,
      MonadIO
    )
  deriving (MonadHexInput) via (HexInputT TestApp)
  deriving (MonadHexState) via (HexStateT TestApp)

instance MonadHexEnv TestApp where
  findAndReadFile (WithImplicitExtension "tfm") "cmr10" = Just <$> liftIO (BS.readFile "test/cmr10.tfm")
  findAndReadFile _ _ = pure Nothing

instance Log.MonadHexLog TestApp where
  log _ _ = pure ()
  logInternalState = pure ()

runTestApp :: ByteString -> TestApp a -> IO (a, AppState)
runTestApp bs app = do
  st <- App.newAppStateWithChars bs
  let io = runExceptT (runStateT (app.unTestApp) st)
  io >>= \case
    Left appError -> panic $ show appError
    Right a -> pure a

evalTestApp :: ByteString -> TestApp a -> IO a
evalTestApp bs app = fst <$> runTestApp bs app

execTestApp :: ByteString -> TestApp a -> IO AppState
execTestApp bs app = snd <$> runTestApp bs app
