module Categorise where

import Test.Tasty
import Test.Tasty.HUnit
import Hexlude
import Hex.Categorise.Types
import Hex.MonadHexState.Interface
import Hex.Codes
import Hex.Categorise.Impl

newtype Error = Error EndOfInput
  deriving stock (Show, Generic)

newtype Mon m a = Mon {unMon :: m a}
  deriving stock (Show, Generic)
  deriving newtype (Functor, Applicative, Monad)


instance Monad m => MonadHexState (Mon m) where
  getIntParameter = panic "NotImplemented"

  getLengthParameter = panic "NotImplemented"

  getGlueParameter = panic "NotImplemented"

  getSpecialLengthParameter = panic "NotImplemented"

  setSpecialLengthParameter = panic "NotImplemented"

  getCategory c = pure $ case c of
    Chr_ '\\' -> Escape
    Chr_ ' ' -> CoreCatCode Space
    Chr_ '%' -> Comment
    Chr_ '\n' -> EndOfLine
    Chr_ '^' -> CoreCatCode Superscript
    Chr_ 'a' -> CoreCatCode Letter
    Chr_ 'b' -> CoreCatCode Letter
    _ -> CoreCatCode Other

  resolveSymbol = panic "NotImplemented"

  loadFont = panic "NotImplemented"

  selectFont = panic "NotImplemented"

  currentFontCharacter = panic "NotImplemented"

  currentFontSpaceGlue = panic "NotImplemented"

tests :: TestTree
tests = testGroup "Categorise"
  [ testCase "Usual" usual
  , testCase "One caret" oneCaret
  , testCase "Two carets" twoCarets
  , testCase "Triod up" triodUp
  , testCase "Triod down" triodDown
  ]

usual :: Assertion
usual = do
  res <- unMon $ charsToCharCats "aa"
  assertEqual "" res
    [ RawCharCat (Chr_ 'a') (CoreCatCode Letter)
    , RawCharCat (Chr_ 'a') (CoreCatCode Letter)
    ]

oneCaret :: Assertion
oneCaret = do
  res <- unMon $ charsToCharCats "^"
  assertEqual "" res
    [ RawCharCat (Chr_ '^') (CoreCatCode Superscript)
    ]
  res2 <- unMon $ charsToCharCats "^a"
  assertEqual "" res2
    [ RawCharCat (Chr_ '^') (CoreCatCode Superscript)
    , RawCharCat (Chr_ 'a') (CoreCatCode Letter)
    ]

twoCarets :: Assertion
twoCarets = do
  res <- unMon $ charsToCharCats "^^"
  assertEqual "" res
    [ RawCharCat (Chr_ '^') (CoreCatCode Superscript)
    , RawCharCat (Chr_ '^') (CoreCatCode Superscript)
    ]

triodUp :: Assertion
triodUp = do
  res <- unMon $ charsToCharCats "^^?"
  assertEqual "" res
    [ RawCharCat (CharCode (63 + 64)) (CoreCatCode Other)
    ]

triodDown :: Assertion
triodDown = do
  res <- unMon $ charsToCharCats "^^A"
  assertEqual "" res
    [ RawCharCat (CharCode (65 - 64)) (CoreCatCode Other)
    ]

  res2 <- unMon $ charsToCharCats "^^^"
  assertEqual "" res2
    [ RawCharCat (CharCode (94 - 64)) (CoreCatCode Other)
    ]
