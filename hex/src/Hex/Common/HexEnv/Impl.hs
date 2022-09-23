{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexEnv.Impl where

import Data.ByteString qualified as BS
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO qualified as FS.IO
import Effectful.Internal.Monad qualified as Eff
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
withHexEnv :: FS.FileSystem :> es => [FilePath] -> Log.LogLevel -> (HexEnv -> Eff es a) -> Eff es a
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

runHexEnv :: [FS.FileSystem, R.Reader HexEnv] :>> es => Eff (EHexEnv : es) a -> Eff es a
runHexEnv = interpret $ \_ -> \case
  FindAndReadFile findPolicy tgtFile -> findAndReadFileImpl findPolicy tgtFile
  FindAndOpenFile findPolicy tgtFile ioMode -> findAndOpenFileImpl findPolicy tgtFile ioMode

findAndReadFileImpl ::
  [Reader HexEnv, FS.FileSystem] :>> es =>
  FindFilePolicy ->
  FilePath ->
  Eff es (Maybe ByteString)
findAndReadFileImpl findPolicy tgtFile = do
  findFilePathImpl findPolicy tgtFile >>= \case
    Nothing -> pure Nothing
    Just absPath -> Just <$> Eff.unsafeEff_ (BS.readFile absPath)

findAndOpenFileImpl ::
  [Reader HexEnv, FS.FileSystem] :>> es =>
  FindFilePolicy ->
  FilePath ->
  IOMode ->
  Eff es (Maybe Handle)
findAndOpenFileImpl findPolicy tgtFile ioMode = do
  findFilePathImpl findPolicy tgtFile >>= \case
    Nothing -> pure Nothing
    Just absPath -> Just <$> FS.IO.openFile absPath ioMode

findFilePathImpl ::
  [Reader HexEnv, FS.FileSystem] :>> es =>
  FindFilePolicy ->
  FilePath ->
  Eff es (Maybe FilePath)
findFilePathImpl findPolicy tgtFile = do
  searchDirs <- know @HexEnv #searchDirs
  FS.findFile searchDirs $ case findPolicy of
    NoImplicitExtension ->
      tgtFile
    WithImplicitExtension ext ->
      Path.replaceExtension tgtFile (toS ext)
