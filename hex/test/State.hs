module State where

import Common qualified as T
import Data.Map.Strict qualified as Map
import Hex.Common.DVI.DocInstruction (FontNumber)
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Grouped (ScopeFlag (..))
import Hex.Common.TFM.Types qualified as TFM
import Hex.Run.App
import Hexlude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Expand"
    [ testCase "Load and select font" testLoadSelectFont
    ]

testLoadSelectFont :: IO ()
testLoadSelectFont = do
  endAppState <- T.execTestApp "" go
  let fontInfos = endAppState.appHexState.fontInfos
  assertEqual "" (Map.size fontInfos) 2
  where
    go :: T.TestApp ()
    go = do
      fontDef <- HSt.loadFont (HexFilePath "cmr10") TFM.NaturalFont
      HSt.selectFont (fontDef ^. typed @FontNumber) Local
