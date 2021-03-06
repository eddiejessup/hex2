module Hex.Lex.Types where

import Data.ByteString qualified as BS
import Hex.Codes qualified as Code
import Hex.Categorise.Types as H.Cat
import Protolude

newtype ControlSequence = ControlSequence ByteString
  deriving newtype (Show, Eq, Hashable)

mkControlSequence :: [Code.CharCode] -> ControlSequence
mkControlSequence csChars = ControlSequence $ BS.pack $ Code.codeWord <$> csChars

data LexSymbol
  = ActiveCharacterSymbol Code.CharCode
  | ControlSequenceSymbol ControlSequence
  deriving stock (Show, Eq, Generic)

instance Hashable LexSymbol

data LexCharCat
  = LexCharCat
      { lexCCChar :: Code.CharCode
      , lexCCCat :: Code.CoreCatCode
      }
  deriving stock (Show, Eq, Generic)

data LexToken
  = CharCatLexToken LexCharCat
  | ControlSequenceLexToken ControlSequence
  deriving stock (Show, Eq, Generic)

data LexState
  = SkippingBlanks
  | LineMiddle
  | LineBegin
  deriving stock (Eq, Show)

data LexError
  = TerminalEscapeCharacter
  | InvalidCharacter
  deriving stock (Show, Eq, Generic)

data LexFailure = LexEndOfInputFailure H.Cat.EndOfInput | LexErrorFailure LexError
  deriving stock (Show, Eq, Generic)

parToken :: LexToken
parToken = ControlSequenceLexToken $ mkControlSequence $ Code.unsafeCodeFromChar <$> ("par" :: [Char])
