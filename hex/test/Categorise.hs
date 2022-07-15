{-# OPTIONS_GHC -Wno-missing-methods #-}

module Categorise where

import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes
import Hex.Common.HexState.Interface
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hex.Common.Quantity qualified as Q
import Hex.Run.App qualified as App
import Hex.Run.Categorise
import Hex.Stage.Categorise.Impl
import Hex.Stage.Categorise.Interface
import Hex.Stage.Categorise.Interface.CharSource
import Hexlude
import Test.Tasty
import Test.Tasty.HUnit

newtype TestApp a = TestApp {unTestApp :: State CharSource a}
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadState CharSource)
  deriving (MonadCharCatSource) via (MonadCharCatSourceT TestApp)

codeToCat :: CharCode -> CatCode
codeToCat = \case
  Chr_ '\\' -> Escape
  Chr_ ' ' -> CoreCatCode Space
  Chr_ '%' -> Comment
  Chr_ '\r' -> EndOfLine
  Chr_ '^' -> CoreCatCode Superscript
  Chr_ 'a' -> CoreCatCode Letter
  Chr_ 'b' -> CoreCatCode Letter
  _ -> CoreCatCode Other

instance Log.MonadHexLog TestApp where
  log _ = pure ()
  logInternalState = pure ()

instance MonadHexState TestApp where
  getHexCode CCatCodeType code = pure $ codeToCat code
  getHexCode _ _ = notImplemented "getHexCode"

  getParameterValue :: Param.QuantParam q -> TestApp (Var.QuantVariableTarget q)
  getParameterValue (Param.IntQuantParam Param.EndLineChar) = pure Q.zeroInt
  getParameterValue _ = notImplemented "getParameterValue"

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
runTestApp bs app =
  let bLines = case App.toInputLines bs of
        Nothing -> panic "bad input"
        Just a -> a

      charSource = newCharSource Nothing bLines
   in evalState (unTestApp app) charSource

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
