module Hex.Stage.Lex.Interface.Extract where

import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as Tx
import Formatting qualified as F
import Hex.Common.Codes qualified as Code
import Hexlude

newtype ControlSequence = ControlSequence {unControlSequence :: ByteString}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

controlSequenceCodes :: ControlSequence -> [Code.CharCode]
controlSequenceCodes cs = BS.unpack (cs.unControlSequence) <&> Code.CharCode

mkControlSequence :: [Code.CharCode] -> ControlSequence
mkControlSequence csChars = ControlSequence $ BS.pack $ Code.unCharCode <$> csChars

fmtControlSequence :: Fmt ControlSequence
fmtControlSequence = "\\" |%| (F.accessed (Tx.decodeUtf8 . (.unControlSequence)) F.stext)

data LexCharCat = LexCharCat
  { lexCCChar :: Code.CharCode,
    lexCCCat :: Code.CoreCatCode
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

lexTokCharCode :: AffineTraversal' LexToken Code.CharCode
lexTokCharCode = lexTokCharCat % typed @Code.CharCode

lexTokCategory :: AffineTraversal' LexToken Code.CoreCatCode
lexTokCategory = lexTokCharCat % typed @Code.CoreCatCode

lexTokCharCat :: Prism' LexToken LexCharCat
lexTokCharCat = _Ctor @"CharCatLexToken"

parToken :: LexToken
parToken = ControlSequenceLexToken $ mkControlSequence $ Code.unsafeCodeFromChar <$> ("par" :: [Char])

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
