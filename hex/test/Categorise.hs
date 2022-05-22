{-# OPTIONS_GHC -Wno-missing-methods #-}

module Categorise where

import Hex.Common.Codes
import Hex.Common.HexState.Interface
import Hex.Run.Categorise
import Hex.Stage.Categorise.Impl
import Hex.Stage.Categorise.Interface
import Hexlude
import Test.Tasty
import Test.Tasty.HUnit

newtype TestApp a = TestApp {unTestApp :: State ByteString a}
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadState ByteString)
  deriving (MonadCharCatSource) via (MonadCharCatSourceT TestApp)

codeToCat :: CharCode -> CatCode
codeToCat = \case
  Chr_ '\\' -> Escape
  Chr_ ' ' -> CoreCatCode Space
  Chr_ '%' -> Comment
  Chr_ '\n' -> EndOfLine
  Chr_ '^' -> CoreCatCode Superscript
  Chr_ 'a' -> CoreCatCode Letter
  Chr_ 'b' -> CoreCatCode Letter
  _ -> CoreCatCode Other

instance MonadHexState TestApp where
  getHexCode CCatCodeType code = pure $ codeToCat code
  getHexCode _ _ = notImplemented "getHexCode"

tests :: TestTree
tests =
  testGroup
    "Categorise"
    [ testCase "Usual" usual,
      testCase "One caret" oneCaret,
      testCase "Two carets" twoCarets,
      testCase "Triod up" triodUp,
      testCase "Triod down" triodDown
    ]

runTestApp :: ByteString -> TestApp a -> a
runTestApp bs app = evalState (unTestApp app) bs

testCategoriseAll :: ByteString -> [RawCharCat]
testCategoriseAll bs = runTestApp bs categoriseAll

usual :: Assertion
usual =
  assertEqual
    ""
    (testCategoriseAll "aa")
    [ RawCharCat (Chr_ 'a') (CoreCatCode Letter),
      RawCharCat (Chr_ 'a') (CoreCatCode Letter)
    ]

oneCaret :: Assertion
oneCaret = do
  assertEqual
    ""
    (testCategoriseAll "^")
    [ RawCharCat (Chr_ '^') (CoreCatCode Superscript)
    ]
  assertEqual
    ""
    (testCategoriseAll "^a")
    [ RawCharCat (Chr_ '^') (CoreCatCode Superscript),
      RawCharCat (Chr_ 'a') (CoreCatCode Letter)
    ]

twoCarets :: Assertion
twoCarets = do
  assertEqual
    ""
    (testCategoriseAll "^^")
    [ RawCharCat (Chr_ '^') (CoreCatCode Superscript),
      RawCharCat (Chr_ '^') (CoreCatCode Superscript)
    ]

triodUp :: Assertion
triodUp = do
  assertEqual
    ""
    (testCategoriseAll "^^?")
    [ RawCharCat (CharCode (63 + 64)) (CoreCatCode Other)
    ]

triodDown :: Assertion
triodDown = do
  assertEqual
    ""
    (testCategoriseAll "^^A")
    [ RawCharCat (CharCode (65 - 64)) (CoreCatCode Other)
    ]
  assertEqual
    ""
    (testCategoriseAll "^^^")
    [ RawCharCat (CharCode (94 - 64)) (CoreCatCode Other)
    ]
