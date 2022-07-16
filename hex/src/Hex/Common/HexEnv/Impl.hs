{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexEnv.Impl where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexEnv.Interface
import Hexlude
import System.Directory qualified as Dir
import System.FilePath qualified as Path
import qualified Data.ByteString as BS

data HexEnv = HexEnv
  { logHandle :: Handle,
    logLevel :: Log.LogLevel,
    searchDirs :: [FilePath]
  }
  deriving stock (Generic)

newHexEnv :: Handle -> Log.LogLevel -> [FilePath] -> HexEnv
newHexEnv logHandle logLevel searchDirs =
  HexEnv
    { logHandle,
      logLevel,
      searchDirs
    }

-- | Set up the enironment in this 'with'-style, to ensure we clean up the log
-- handle when we're finished.
withHexEnv :: [FilePath] -> Log.LogLevel -> (HexEnv -> IO a) -> IO a
withHexEnv extraSearchDirs logLevel k = do
  cwd <- Dir.getCurrentDirectory
  let searchDirs =
        extraSearchDirs ++ [ cwd,
          "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/cm/",
          "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/knuth-lib",
          "/usr/local/texlive/2022/texmf-dist/tex/generic/hyphen"
        ]
  withFile "log.txt" WriteMode $ \hexLogHandle -> do
    k $ newHexEnv hexLogHandle logLevel searchDirs

findFilePathImpl :: MonadIO m => FindFilePolicy -> [FilePath] -> FilePath -> m (Maybe FilePath)
findFilePathImpl findPolicy dirs tgtFile = do
  let tgtFileName = case findPolicy of
        NoImplicitExtension ->
          tgtFile
        WithImplicitExtension ext ->
          Path.replaceExtension tgtFile (toS ext)
  liftIO $ Dir.findFile dirs tgtFileName

newtype HexEnvT m a = HexEnvT {unMonadHexInputImplT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState st,
      MonadReader r,
      MonadError e,
      Log.MonadHexLog
    )

instance (Monad (HexEnvT m), MonadIO (HexEnvT m), MonadReader e (HexEnvT m), HasType HexEnv e) => MonadHexEnv (HexEnvT m) where
  findAndReadFile findPolicy tgtFile = do
    searchDirs <- know (typed @HexEnv % typed @[FilePath])
    findFilePathImpl findPolicy searchDirs tgtFile >>= \case
      Nothing -> pure Nothing
      Just absPath -> Just <$> liftIO (BS.readFile absPath)
