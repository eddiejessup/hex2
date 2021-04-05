module Lex where

import Test.Tasty
import Test.Tasty.HUnit
import Hexlude

import Categorise qualified as Cat
import Hex.Lex.Impl
import Hex.Lex.Types
import Hex.Codes

tests :: TestTree
tests = testGroup "Lex tests"
  [ testCase "Chars" chars
  , testCase "Words" words
  , testCase "Multiple spaces" multipleSpaces
  , testCase "Comment" comment
  , testCase "Spaces at beginning of line" spacesLineBegin
  , testCase "New-line while skipping blanks" newLineSkippingBlanks
  , controlSequenceTests
  ]

assertSuccessLexEqual :: ByteString -> [LexToken] -> IO ()
assertSuccessLexEqual s ts = do
  res <- Cat.unMon $ runExceptT $ charsToLexTokens s
  assertEqual "" (Right ts) res

assertFailedLex :: ByteString -> IO ()
assertFailedLex s = do
  res <- Cat.unMon $ runExceptT $ charsToLexTokens s
  assertEqual "" (Left TerminalEscapeCharacter) res

letter :: Char -> LexToken
letter c = CharCatLexToken (LexCharCat (Chr_ c) Letter)

space :: LexToken
space = CharCatLexToken (LexCharCat (Chr_ ' ') Space)

chars :: Assertion
chars = do
  assertSuccessLexEqual "aa"
    [ letter 'a'
    , letter 'a'
    ]

words :: Assertion
words = do
  assertSuccessLexEqual "aa aa aa"
    [ letter 'a'
    , letter 'a'
    , space
    , letter 'a'
    , letter 'a'
    , space
    , letter 'a'
    , letter 'a'
    ]

multipleSpaces :: Assertion
multipleSpaces = do
  assertSuccessLexEqual "aa     aa"
    [ letter 'a'
    , letter 'a'
    , space
    , letter 'a'
    , letter 'a'
    ]

comment :: Assertion
comment = do
  assertSuccessLexEqual "aa%c1c1c1\nbb"
    [ letter 'a'
    , letter 'a'
    , letter 'b'
    , letter 'b'
    ]

spacesLineBegin :: Assertion
spacesLineBegin = do
  assertSuccessLexEqual "   aa"
    [ letter 'a'
    , letter 'a'
    ]

newLineSkippingBlanks :: Assertion
newLineSkippingBlanks = do
  assertSuccessLexEqual "a  \na"
    [ letter 'a'
    , space
    , letter 'a'
    ]

controlSequenceTests :: TestTree
controlSequenceTests = testGroup "Control word"
  [ testCase "Alone" $ assertSuccessLexEqual
      "\\abab"
      [ ControlSequenceLexToken (ControlSequence "abab")
      ]
  , testCase "Control word with following digit" $ assertSuccessLexEqual "\\abab1"
    [ ControlSequenceLexToken (ControlSequence "abab")
    , CharCatLexToken (LexCharCat (Chr_ '1') Other)
    ]
  , testCase "Control word with following space" $ assertSuccessLexEqual "\\abab "
    [ ControlSequenceLexToken (ControlSequence "abab")
    ]
  , testCase "Control letter-character alone" $ assertSuccessLexEqual "\\a"
    [ ControlSequenceLexToken (ControlSequence "a")
    ]
  , testCase "Control letter-character with following digit" $ assertSuccessLexEqual "\\a1"
    [ ControlSequenceLexToken (ControlSequence "a")
    , CharCatLexToken (LexCharCat (Chr_ '1') Other)
    ]
  , testCase "Control letter-character with following space" $ assertSuccessLexEqual "\\a "
    [ ControlSequenceLexToken (ControlSequence "a")
    ]
  , testCase "Control other-character alone" $ assertSuccessLexEqual "\\1"
    [ ControlSequenceLexToken (ControlSequence "1")
    ]
  , testCase "Control other-character with following digit" $ assertSuccessLexEqual "\\11"
    [ ControlSequenceLexToken (ControlSequence "1")
    , CharCatLexToken (LexCharCat (Chr_ '1') Other)
    ]
  , testCase "Control other-character with following space" $ assertSuccessLexEqual "\\1 "
    [ ControlSequenceLexToken (ControlSequence "1")
    , space
    ]
  , testCase "Terminal escape character" $ assertFailedLex "\\"
  ]
