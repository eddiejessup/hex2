module Hex.Stage.Lex.Interface.Extract where

import Data.ByteString qualified as BS
import Data.Generics.Product (getTyped)
import Data.Text.Encoding qualified as Tx
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hex.Common.Codes qualified as Codes
import Hexlude

newtype ControlSequence = ControlSequence ByteString
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

mkControlSequence :: [Codes.CharCode] -> ControlSequence
mkControlSequence csChars = ControlSequence $ BS.pack $ Codes.unCharCode <$> csChars

fmtControlSequence :: Fmt ControlSequence
fmtControlSequence = "\\" |%| (F.accessed (Tx.decodeUtf8 . (getTyped @ByteString)) F.stext)

data LexCharCat = LexCharCat
  { lexCCChar :: Codes.CharCode,
    lexCCCat :: Codes.CoreCatCode
  }
  deriving stock (Show, Eq, Generic)

fmtLexCharCat :: Fmt LexCharCat
fmtLexCharCat =
  let f1 = fmtLexCharCatChar
      f2 = F.accessed (.lexCCCat) Code.fmtCoreCatCode
   in F.parenthesised $ f1 <> F.fconst ", " <> f2

fmtLexCharCatChar :: Fmt LexCharCat
fmtLexCharCatChar =
  F.accessed (.lexCCChar) Code.fmtCharCode

data LexToken
  = CharCatLexToken LexCharCat
  | ControlSequenceLexToken ControlSequence
  deriving stock (Show, Eq, Generic)

fmtLexToken :: Fmt LexToken
fmtLexToken = later $ \case
  CharCatLexToken lexCC ->
    bformat fmtLexCharCat lexCC
  ControlSequenceLexToken controlSeq ->
    bformat fmtControlSequence controlSeq

fmtLexTokenChar :: Fmt LexToken
fmtLexTokenChar = later $ \case
  CharCatLexToken lexCC ->
    bformat fmtLexCharCatChar lexCC
  ControlSequenceLexToken controlSeq ->
    bformat fmtControlSequence controlSeq

lexTokCharCode :: AffineTraversal' LexToken Codes.CharCode
lexTokCharCode = lexTokCharCat % typed @Codes.CharCode

lexTokCategory :: AffineTraversal' LexToken Codes.CoreCatCode
lexTokCategory = lexTokCharCat % typed @Codes.CoreCatCode

lexTokCharCat :: Prism' LexToken LexCharCat
lexTokCharCat = _Ctor @"CharCatLexToken"

parToken :: LexToken
parToken = ControlSequenceLexToken $ mkControlSequence $ Codes.unsafeCodeFromChar <$> ("par" :: [Char])

data LexState
  = SkippingBlanks
  | LineMiddle
  | LineBegin
  deriving stock (Eq, Show)

data LexError
  = TerminalEscapeCharacter
  | InvalidCharacter
  deriving stock (Show, Eq, Generic)

fmtLexError :: Fmt LexError
fmtLexError = F.shown
