module Hex.Common.HexEnv.Interface where

import Hexlude
import System.Directory qualified as Dir
import System.FilePath qualified as Path

data HexEnv = HexEnv
  { logHandle :: Handle,
    searchDirs :: [FilePath]
  }
  deriving stock (Generic)

newHexEnv :: Handle -> HexEnv
newHexEnv logHandle =
  HexEnv
    { logHandle,
      searchDirs =
        [ "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/cm/",
          "/usr/local/texlive/2022/texmf-dist/fonts/tfm/public/knuth-lib"
        ]
    }

-- | Set up the enironment in this 'with'-style, to ensure we clean up the log
-- handle when we're finished.
withHexEnv :: (HexEnv -> IO a) -> IO a
withHexEnv k = do
  withFile "log.txt" WriteMode $ \hexLogHandle -> do
    k $ newHexEnv hexLogHandle

data FindFilePolicy
  = NoImplicitExtension
  | WithImplicitExtension Text

findFilePath :: MonadIO m => FindFilePolicy -> [FilePath] -> FilePath -> m (Maybe FilePath)
findFilePath findPolicy dirs tgtFile = do
  let tgtFileName = case findPolicy of
        NoImplicitExtension ->
          tgtFile
        WithImplicitExtension ext ->
          Path.replaceExtension tgtFile (toS ext)
  liftIO $ Dir.findFile dirs tgtFileName
