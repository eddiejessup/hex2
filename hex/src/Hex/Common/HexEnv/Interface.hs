{-# LANGUAGE UndecidableInstances #-}
module Hex.Common.HexEnv.Interface where

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
  let
    searchDirs =
      [ cwd,
        "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/cm/",
        "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/knuth-lib",
        "/usr/local/texlive/2022/texmf-dist/tex/generic/hyphen"
      ]
  withFile "log.txt" WriteMode $ \hexLogHandle -> do
    k $ newHexEnv hexLogHandle searchDirs

data FindFilePolicy
  = NoImplicitExtension
  | WithImplicitExtension Text

findFilePathImpl :: MonadIO m => FindFilePolicy -> [FilePath] -> FilePath -> m (Maybe FilePath)
findFilePathImpl findPolicy dirs tgtFile = do
  let tgtFileName = case findPolicy of
        NoImplicitExtension ->
          tgtFile
        WithImplicitExtension ext ->
          Path.replaceExtension tgtFile (toS ext)
  liftIO $ Dir.findFile dirs tgtFileName

class Monad m => MonadHexEnv m where
  findFilePath :: FindFilePolicy -> FilePath -> m (Maybe FilePath)

instance (Monad m, MonadIO m, MonadReader e m, HasType HexEnv e) => MonadHexEnv m where
  findFilePath findPolicy tgtFile = do
    searchDirs <- know (typed @HexEnv % typed @[FilePath])
    findFilePathImpl findPolicy searchDirs tgtFile
