{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexEnv.Impl where

import Data.ByteString qualified as BS
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO qualified as FS.IO
import Effectful.Reader.Dynamic qualified as R
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexEnv.Interface
import Hexlude
import System.FilePath qualified as Path

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
withHexEnv :: (FS.FileSystem :> es) => [FilePath] -> Log.LogLevel -> (HexEnv -> Eff es a) -> Eff es a
withHexEnv extraSearchDirs logLevel k = do
  cwd <- FS.getCurrentDirectory
  let searchDirs =
        extraSearchDirs
          ++ [ cwd,
               "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/cm/",
               "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/knuth-lib",
               "/usr/local/texlive/2022/texmf-dist/tex/generic/hyphen"
             ]
  FS.IO.withFile "log.txt" WriteMode $ \hexLogHandle -> do
    k $ newHexEnv hexLogHandle logLevel searchDirs

runHexEnv :: (FS.FileSystem :> es, R.Reader HexEnv :> es, IOE :> es) => Eff (EHexEnv : es) a -> Eff es a
runHexEnv = interpret $ \_ -> \case
  FindAndReadFile findPolicy tgtFile -> findAndReadFileImpl findPolicy tgtFile
  FindAndOpenFile findPolicy tgtFile ioMode -> findAndOpenFileImpl findPolicy tgtFile ioMode

findAndReadFileImpl ::
  (Reader HexEnv :> es, FS.FileSystem :> es, IOE :> es) =>
  FindFilePolicy ->
  HexFilePath ->
  Eff es (Maybe ByteString)
findAndReadFileImpl findPolicy tgtFile = do
  findFilePathImpl findPolicy tgtFile >>= \case
    Nothing -> pure Nothing
    Just absPath -> Just <$> (liftIO $ BS.readFile absPath)

findAndOpenFileImpl ::
  (Reader HexEnv :> es, FS.FileSystem :> es) =>
  FindFilePolicy ->
  HexFilePath ->
  IOMode ->
  Eff es (Maybe Handle)
findAndOpenFileImpl findPolicy tgtFile ioMode = do
  findFilePathImpl findPolicy tgtFile >>= \case
    Nothing -> pure Nothing
    Just absPath -> Just <$> FS.IO.openFile absPath ioMode

findFilePathImpl ::
  (Reader HexEnv :> es, FS.FileSystem :> es) =>
  FindFilePolicy ->
  HexFilePath ->
  Eff es (Maybe FilePath)
findFilePathImpl findPolicy tgtFile = do
  searchDirs <- know @HexEnv #searchDirs
  let tgtFilePath = tgtFile.unHexFilePath
  FS.findFile searchDirs $ case findPolicy of
    NoImplicitExtension ->
      tgtFilePath
    WithImplicitExtension ext ->
      Path.replaceExtension tgtFilePath (toS ext)
