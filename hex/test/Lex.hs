{-# OPTIONS_GHC -Wno-missing-methods #-}

module Lex where

import Categorise qualified as Test.Cat
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes
import Hex.Common.HexState.Interface (MonadHexState (..))
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hex.Run.App qualified as App
import Hex.Run.Lex (lexAll)
import Hex.Stage.Categorise.Impl qualified as Cat
import Hex.Stage.Categorise.Interface qualified as Cat
import Hex.Stage.Lex.Impl.Extract (extractToken)
import Hex.Stage.Lex.Interface
import Hex.Stage.Lex.Interface
import Hex.Stage.Lex.Interface.LexBuffer qualified as Lex
import Hexlude
import Test.Tasty
import Test.Tasty.HUnit

newtype TestApp a = TestApp {unTestApp :: State Lex.LexBuffer a}
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadState Lex.LexBuffer)
  deriving (Cat.MonadCharCatSource) via (Cat.MonadCharCatSourceT TestApp)

runTestApp :: ByteString -> TestApp a -> a
runTestApp bs app =
  let bLines = case App.toInputLines bs of
        Nothing -> panic "bad input"
        Just a -> a

      lexBuffer = Lex.newLexBuffer (Just App.carriageReturnCharCode) bLines
   in evalState (unTestApp app) lexBuffer

testLexAll :: ByteString -> [LexToken]
testLexAll bs = runTestApp bs lexAll

instance Log.MonadHexLog TestApp where
  log _ = pure ()
  logInternalState = pure ()

instance MonadHexState TestApp where
  getHexCode CCatCodeType code = pure $ Test.Cat.codeToCat code
  getHexCode _ _ = notImplemented "getHexCode"

  getParameterValue :: Param.QuantParam q -> TestApp (Var.QuantVariableTarget q)
  getParameterValue (Param.IntQuantParam Param.EndLineChar) = pure $ toHexInt App.carriageReturnCharCode
  getParameterValue _ = notImplemented "getParameterValue"

extractLexToken :: TestApp (Maybe LexToken)
extractLexToken = do
  s <- get
  runExceptT @(Identity LexError) (extractToken s.bufferLineState) >>= \case
    Left e ->
      panic $ show e
    Right Nothing ->
      pure Nothing
    Right (Just (lt, newLineState)) -> do
      assign' (#bufferLineState) newLineState
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
      testControlSequences
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
testChars =
  assertSuccessLexEqual
    "aa"
    [ letter 'a',
      letter 'a',
      space
    ]

testWords :: Assertion
testWords =
  assertSuccessLexEqual
    "aa aa aa"
    [ letter 'a',
      letter 'a',
      space,
      letter 'a',
      letter 'a',
      space,
      letter 'a',
      letter 'a',
      space
    ]

testMultipleSpaces :: Assertion
testMultipleSpaces =
  assertSuccessLexEqual
    "aa     aa"
    [ letter 'a',
      letter 'a',
      space,
      letter 'a',
      letter 'a',
      space
    ]

testComment :: Assertion
testComment =
  assertSuccessLexEqual
    "aa%c1c1c1\nbb"
    [ letter 'a',
      letter 'a',
      letter 'b',
      letter 'b',
      space
    ]

testSpacesLineBegin :: Assertion
testSpacesLineBegin =
  assertSuccessLexEqual
    "   aa"
    [ letter 'a',
      letter 'a',
      space
    ]

testNewLineSkippingBlanks :: Assertion
testNewLineSkippingBlanks =
  assertSuccessLexEqual
    "a  \na"
    [ letter 'a',
      space,
      letter 'a',
      space
    ]

testControlSequences :: TestTree
testControlSequences =
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
            CharCatLexToken (LexCharCat (Chr_ '1') Other),
            space
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
            CharCatLexToken (LexCharCat (Chr_ '1') Other),
            space
          ],
      testCase "Control letter-character with following space" $
        assertSuccessLexEqual
          "\\a "
          [ ControlSequenceLexToken (ControlSequence "a")
          ],
      testCase "Control other-character alone" $
        assertSuccessLexEqual
          "\\1"
          [ ControlSequenceLexToken (ControlSequence "1"),
            space
          ],
      testCase "Control other-character with following digit" $
        assertSuccessLexEqual
          "\\11"
          [ ControlSequenceLexToken (ControlSequence "1"),
            CharCatLexToken (LexCharCat (Chr_ '1') Other),
            space
          ],
      testCase "Control other-character with following space" $
        assertSuccessLexEqual
          "\\1 "
          [ ControlSequenceLexToken (ControlSequence "1"),
            space
          ]
    ]
