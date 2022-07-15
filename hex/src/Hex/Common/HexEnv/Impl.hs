{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexEnv.Impl where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexEnv.Interface
import Hexlude
import System.Directory qualified as Dir
import System.FilePath qualified as Path

data HexEnv = HexEnv
  { logHandle :: Handle,
    searchDirs :: [FilePath]
  }
  deriving stock (Generic)

newHexEnv :: Handle -> [FilePath] -> HexEnv
newHexEnv logHandle searchDirs =
  HexEnv
    { logHandle,
      searchDirs
    }

-- | Set up the enironment in this 'with'-style, to ensure we clean up the log
-- handle when we're finished.
withHexEnv :: (HexEnv -> IO a) -> IO a
withHexEnv k = do
  cwd <- Dir.getCurrentDirectory
  let searchDirs =
        [ cwd,
          "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/cm/",
          "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/knuth-lib",
          "/usr/local/texlive/2022/texmf-dist/tex/generic/hyphen"
        ]
  withFile "log.txt" WriteMode $ \hexLogHandle -> do
    k $ newHexEnv hexLogHandle searchDirs

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
  findFilePath findPolicy tgtFile = do
    searchDirs <- know (typed @HexEnv % typed @[FilePath])
    findFilePathImpl findPolicy searchDirs tgtFile
