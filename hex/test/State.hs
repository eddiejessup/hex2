{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module State where

import Common qualified as T
import Data.Map.Strict qualified as Map
import Hex.Common.HexState.Impl.Type qualified as HSt
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Font (FontNumber (..))
import Hex.Common.HexState.Interface.Grouped (ScopeFlag (..))
import Hex.Common.Quantity qualified as Q
import Hex.Run.App
import Hex.Stage.Build.BoxElem qualified as H.Inter.B.Box
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
      fontDef <- HSt.loadFont (Q.HexFilePath "cmr10") H.Inter.B.Box.NaturalFont
      HSt.selectFont (fontDef ^. typed @FontNumber) Local
