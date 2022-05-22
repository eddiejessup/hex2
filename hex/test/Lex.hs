{-# OPTIONS_GHC -Wno-missing-methods #-}

module Lex where

import Categorise qualified as Test.Cat
import Hex.Common.Codes
import Hex.Common.HexState.Interface (MonadHexState (..))
import Hex.Run.Lex (lexAll)
import Hex.Stage.Lex.Impl.Extract (extractToken)
import Hex.Stage.Lex.Interface
import Hex.Stage.Lex.Interface.Extract
import Hexlude
import Test.Tasty
import Test.Tasty.HUnit

data TestAppState = TestAppState {chars :: ByteString, lexState :: LexState}
  deriving stock (Generic)

newTestAppState :: ByteString -> TestAppState
newTestAppState bs = TestAppState bs LineBegin

newtype TestApp a = TestApp {unTestApp :: State TestAppState a}
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadState TestAppState)

runTestApp :: ByteString -> TestApp a -> a
runTestApp bs app = evalState (unTestApp app) (newTestAppState bs)

testLexAll :: ByteString -> [LexToken]
testLexAll bs = runTestApp bs lexAll

instance MonadHexState TestApp where
  getHexCode CCatCodeType code = pure $ Test.Cat.codeToCat code
  getHexCode _ _ = notImplemented "getHexCode"

extractLexToken :: TestApp (Maybe LexToken)
extractLexToken = do
  s <- get
  runExceptT @(Identity LexError) (extractToken s.lexState s.chars) >>= \case
    Left e ->
      panic $ show e
    Right Nothing ->
      pure Nothing
    Right (Just (lt, newLexState, newChars)) -> do
      assign' (#lexState) newLexState
      assign' (#chars) newChars
      pure $ Just lt

instance MonadLexTokenSource TestApp where
  getLexToken = extractLexToken

tests :: TestTree
tests =
  testGroup
    "Lex"
    [ testCase "Chars" testChars,
      testCase "Words" testWords,
      testCase "Multiple spaces" testMultipleSpaces,
      testCase "Comment" testComment,
      testCase "Spaces at beginning of line" testSpacesLineBegin,
      testCase "New-line while skipping blanks" testNewLineSkippingBlanks,
      testControlSequenceTests
    ]

assertSuccessLexEqual :: ByteString -> [LexToken] -> IO ()
assertSuccessLexEqual s ts =
  assertEqual "" ts (testLexAll s)

-- assertFailedLex :: ByteString -> IO ()
-- assertFailedLex s = do
--   res <- Cat.unMon $ runExceptT $ charsToLexTokens s
--   assertEqual "" (Left TerminalEscapeCharacter) res

letter :: Char -> LexToken
letter c = CharCatLexToken (LexCharCat (Chr_ c) Letter)

space :: LexToken
space = CharCatLexToken (LexCharCat (Chr_ ' ') Space)

testChars :: Assertion
testChars = do
  assertSuccessLexEqual
    "aa"
    [ letter 'a',
      letter 'a'
    ]

testWords :: Assertion
testWords = do
  assertSuccessLexEqual
    "aa aa aa"
    [ letter 'a',
      letter 'a',
      space,
      letter 'a',
      letter 'a',
      space,
      letter 'a',
      letter 'a'
    ]

testMultipleSpaces :: Assertion
testMultipleSpaces = do
  assertSuccessLexEqual
    "aa     aa"
    [ letter 'a',
      letter 'a',
      space,
      letter 'a',
      letter 'a'
    ]

testComment :: Assertion
testComment = do
  assertSuccessLexEqual
    "aa%c1c1c1\nbb"
    [ letter 'a',
      letter 'a',
      letter 'b',
      letter 'b'
    ]

testSpacesLineBegin :: Assertion
testSpacesLineBegin = do
  assertSuccessLexEqual
    "   aa"
    [ letter 'a',
      letter 'a'
    ]

testNewLineSkippingBlanks :: Assertion
testNewLineSkippingBlanks = do
  assertSuccessLexEqual
    "a  \na"
    [ letter 'a',
      space,
      letter 'a'
    ]

testControlSequenceTests :: TestTree
testControlSequenceTests =
  testGroup
    "Control word"
    [ testCase "Alone" $
        assertSuccessLexEqual
          "\\abab"
          [ ControlSequenceLexToken (ControlSequence "abab")
          ],
      testCase "Control word with following digit" $
        assertSuccessLexEqual
          "\\abab1"
          [ ControlSequenceLexToken (ControlSequence "abab"),
            CharCatLexToken (LexCharCat (Chr_ '1') Other)
          ],
      testCase "Control word with following space" $
        assertSuccessLexEqual
          "\\abab "
          [ ControlSequenceLexToken (ControlSequence "abab")
          ],
      testCase "Control letter-character alone" $
        assertSuccessLexEqual
          "\\a"
          [ ControlSequenceLexToken (ControlSequence "a")
          ],
      testCase "Control letter-character with following digit" $
        assertSuccessLexEqual
          "\\a1"
          [ ControlSequenceLexToken (ControlSequence "a"),
            CharCatLexToken (LexCharCat (Chr_ '1') Other)
          ],
      testCase "Control letter-character with following space" $
        assertSuccessLexEqual
          "\\a "
          [ ControlSequenceLexToken (ControlSequence "a")
          ],
      testCase "Control other-character alone" $
        assertSuccessLexEqual
          "\\1"
          [ ControlSequenceLexToken (ControlSequence "1")
          ],
      testCase "Control other-character with following digit" $
        assertSuccessLexEqual
          "\\11"
          [ ControlSequenceLexToken (ControlSequence "1"),
            CharCatLexToken (LexCharCat (Chr_ '1') Other)
          ],
      testCase "Control other-character with following space" $
        assertSuccessLexEqual
          "\\1 "
          [ ControlSequenceLexToken (ControlSequence "1"),
            space
          ]
          -- testCase "Terminal escape character" $ assertFailedLex "\\"
    ]
