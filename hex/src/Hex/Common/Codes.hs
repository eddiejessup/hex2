{-# LANGUAGE ViewPatterns #-}

module Hex.Common.Codes where

import ASCII qualified
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hexlude

class HexCode a where
  toHexInt :: a -> Q.HexInt

  fromHexInt :: Q.HexInt -> Maybe a

newtype CharCode = CharCode {unCharCode :: Word8}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum, Bounded, Hashable)

pattern Chr_ :: Char -> CharCode
pattern Chr_ c <-
  (unsafeCodeAsChar -> c)
  where
    Chr_ c = unsafeCodeFromChar c

codeAsAsciiChar :: CharCode -> ASCII.Char
codeAsAsciiChar code =
  -- We should never get hold of a char-code that isn't a valid ASCII character.
  ASCII.word8ToCharUnsafe code.unCharCode

unsafeCodeAsChar :: CharCode -> Char
unsafeCodeAsChar (CharCode w) = case ASCII.word8ToCharMaybe w of
  Just asciiChar -> ASCII.charToUnicode asciiChar
  Nothing -> chr $ fromIntegral w

unsafeCodeFromChar :: Char -> CharCode
unsafeCodeFromChar c = case ASCII.unicodeToCharMaybe c of
  Just asciiChar -> CharCode $ ASCII.charToWord8 asciiChar
  Nothing -> panic $ show c

fmtCharCode :: Fmt CharCode
fmtCharCode = F.accessed (\code -> (Char.showLitChar $ unsafeCodeAsChar code) "") F.string

codeFromAsciiChar :: ASCII.Char -> CharCode
codeFromAsciiChar = CharCode . ASCII.charToWord8

textAsCharCodes :: Text -> [CharCode]
textAsCharCodes = bytesAsCharCodes . encodeUtf8

bytesAsCharCodes :: ByteString -> [CharCode]
bytesAsCharCodes x = BS.unpack x <&> CharCode

codesAsText :: [CharCode] -> Text
codesAsText cs = decodeUtf8 $ BS.pack $ cs <&> unCharCode

codeInt :: CharCode -> Int
codeInt = fromIntegral . unCharCode

instance HexCode CharCode where
  toHexInt charCode = Q.HexInt $ charCode ^. #unCharCode % to (fromIntegral @Word8 @Int)

  fromHexInt (Q.HexInt n)
    | n > 256 = Nothing
    | n < 0 = Nothing
    | otherwise = Just $ CharCode $ fromIntegral @Int @Word8 n

readCharCodes :: IOE :> es => FilePath -> Eff es ByteString
readCharCodes = liftIO . BS.readFile

-- Category code
-----------------

data CatCode
  = Escape -- 0
  | EndOfLine -- 5
  | Ignored -- 9
  | Comment -- 14
  | Invalid -- 15
  | CoreCatCode CoreCatCode
  deriving stock (Show, Eq, Generic)

fmtCatCode :: Fmt CatCode
fmtCatCode = F.later $ \case
  CoreCatCode x -> F.bformat fmtCoreCatCode x
  x -> F.bformat F.shown x

instance HexCode CatCode where
  toHexInt catCode = Q.HexInt $ case catCode of
    Escape -> 0
    CoreCatCode BeginGroup -> 1
    CoreCatCode EndGroup -> 2
    CoreCatCode MathShift -> 3
    CoreCatCode AlignTab -> 4
    EndOfLine -> 5
    CoreCatCode Parameter -> 6
    CoreCatCode Superscript -> 7
    CoreCatCode Subscript -> 8
    Ignored -> 9
    CoreCatCode Space -> 10
    CoreCatCode Letter -> 11
    CoreCatCode Other -> 12
    CoreCatCode Active -> 13
    Comment -> 14
    Invalid -> 15

  fromHexInt (Q.HexInt n) = case n of
    0 -> Just Escape
    1 -> Just $ CoreCatCode BeginGroup
    2 -> Just $ CoreCatCode EndGroup
    3 -> Just $ CoreCatCode MathShift
    4 -> Just $ CoreCatCode AlignTab
    5 -> Just EndOfLine
    6 -> Just $ CoreCatCode Parameter
    7 -> Just $ CoreCatCode Superscript
    8 -> Just $ CoreCatCode Subscript
    9 -> Just Ignored
    10 -> Just $ CoreCatCode Space
    11 -> Just $ CoreCatCode Letter
    12 -> Just $ CoreCatCode Other
    13 -> Just $ CoreCatCode Active
    14 -> Just Comment
    15 -> Just Invalid
    _ -> Nothing

-- Not all Catcodes make it past the lexer.
data CoreCatCode
  = BeginGroup -- 1
  | EndGroup -- 2
  | MathShift -- 3
  | AlignTab -- 4
  | Parameter -- 6
  | Superscript -- 7
  | Subscript -- 8
  | Space -- 10
  | Letter -- 11
  | Other -- 12
  | Active -- 13
  deriving stock (Show, Eq, Generic)

fmtCoreCatCode :: Fmt CoreCatCode
fmtCoreCatCode = F.shown

-- Delimiter code.
------------------

-- The ⟨number⟩ at the end of a ⟨code assignment⟩ must not be negative, except
-- in the case that a \delcode is being assigned.
-- Furthermore, that ⟨number⟩
-- should be at most 15 for \catcode, 32768 for \mathcode, 255 for \lccode or
-- \uccode, 32767 for \sfcode, and 2^24 − 1 for \delcode.
-- Delimiter code.
-- a delcode is either negative, for characters that should not act as
-- delimiters, or less than "1000000.
-- In other words, non-negative delcodes consist of six hexadecimal digits.
-- The first and last sets of three digits specify "small" and "large" variants
-- of the delimiter, respectively.
-- the code,
--     "123456
-- implies a small variant in position "23 of family "1, and a large variant in
-- position "56 of family "4.
-- If the small or large variant is given as "000, however (position 0 of
-- family 0), that variant is ignored.
data DelimiterCode = NotADelimiter Q.HexInt | DelimiterSpecCode DelimiterSpec
  deriving stock (Show, Eq)

fmtDelimiterCode :: Fmt DelimiterCode
fmtDelimiterCode = F.later $ \case
  NotADelimiter n -> "Not a delimiter " <> F.bformat (F.parenthesised Q.fmtHexInt) n
  DelimiterSpecCode spec -> "Delimiter with spec: " <> F.bformat fmtDelimiterSpec spec

instance HexCode DelimiterCode where
  toHexInt (NotADelimiter n) = n
  toHexInt (DelimiterSpecCode DelimiterSpec {smallVar, largeVar}) =
    let (Q.HexInt largeVarN) = toHexInt largeVar
        (Q.HexInt smallVarN) = toHexInt smallVar
     in Q.HexInt $ largeVarN `shiftL` 12 + smallVarN

  fromHexInt (Q.HexInt n)
    | n < 0 = Just $ NotADelimiter $ Q.HexInt n
    | n > 0xFFFFFF = Nothing
    | otherwise =
        do
          smallVar <- fromHexInt $ Q.HexInt $ n `shiftR` 12
          largeVar <- fromHexInt $ Q.HexInt $ n .&. 0xFFF
          Just $ DelimiterSpecCode $ DelimiterSpec smallVar largeVar

data DelimiterSpec = DelimiterSpec {smallVar, largeVar :: DelimiterVar}
  deriving stock (Show, Eq)

fmtDelimiterSpec :: Fmt DelimiterSpec
fmtDelimiterSpec =
  ("Small variant: " |%| F.accessed (.smallVar) fmtDelimiterVar |%| ", ")
    <> ("Large variant: " |%| F.accessed (.largeVar) fmtDelimiterVar)

data DelimiterVar = PresentDelimiterVar FamilyCharRef | NullDelimiterVar
  deriving stock (Show, Eq)

fmtDelimiterVar :: Fmt DelimiterVar
fmtDelimiterVar = F.later $ \case
  PresentDelimiterVar familyCharRef -> "Present-delimiter " <> F.bformat F.shown familyCharRef
  NullDelimiterVar -> "Null-delimiter-variant"

instance HexCode DelimiterVar where
  toHexInt NullDelimiterVar = Q.HexInt 0
  toHexInt (PresentDelimiterVar f) = toHexInt f

  fromHexInt h@(Q.HexInt n)
    | n == 0 = Just NullDelimiterVar
    | otherwise = PresentDelimiterVar <$> fromHexInt h

data FamilyCharRef = FamilyCharRef {family :: Q.HexInt, position :: CharCode}
  deriving stock (Show, Eq, Generic)

instance HexCode FamilyCharRef where
  toHexInt (FamilyCharRef (Q.HexInt famN) pos) = Q.HexInt $ (famN `shiftL` 8) + (toHexInt pos ^. typed @Int)

  fromHexInt (Q.HexInt n)
    | n < 0 = Nothing
    | n > 0xFFF = Nothing
    | otherwise = do
        pos <- fromHexInt (Q.HexInt (n .&. 0xFF))
        pure $ FamilyCharRef (Q.HexInt $ n `shiftR` 8) pos

-- Math code.
-- ----------

-- A math-code is specified by a number between 0 and 4095.
-- Consider such a number, represented as four hexadecimal digits: "pfcc
-- p: MathClass
-- f: font family
-- cc: character code position
-- A mathcode can also have the special value "8000, which causes the character
-- to behave as if it has catcode 13 (active).
data MathCode = NormalMathCode MathClass FamilyCharRef | ActiveMathCode
  deriving stock (Show, Eq, Generic)

fmtMathCode :: Fmt MathCode
fmtMathCode = F.shown

instance HexCode MathCode where
  toHexInt ActiveMathCode =
    Q.HexInt 0x8000
  toHexInt (NormalMathCode cls famRef) =
    let (Q.HexInt famRefN) = toHexInt famRef
        (Q.HexInt clsN) = toHexInt cls
     in Q.HexInt $ famRefN + (clsN `shiftL` 12)

  fromHexInt (Q.HexInt n)
    | n < 0 = Nothing
    | n > 0x8000 = Nothing
    | n == 0x8000 = Just ActiveMathCode
    | otherwise = do
        cls <- fromHexInt (Q.HexInt $ n `shiftR` 12)
        famRef <- fromHexInt (Q.HexInt $ n .&. 0xFFF)
        pure $ NormalMathCode cls famRef

data MathClass
  = Ordinary -- 0
  | LargeOperator -- 1
  | BinaryRelation -- 2
  | Relation -- 3
  | Opening -- 4
  | Closing -- 5
  | Punctuation -- 6
  | VariableFamily -- 7
  deriving stock (Show, Eq, Enum, Bounded)

instance HexCode MathClass where
  toHexInt = Q.HexInt . fromEnum

  fromHexInt = Just . toEnum . Q.unHexInt

-- Change case code.
-- -----------------

-- Conversion to uppercase means that a character is replaced by its \uccode
-- value, unless the \uccode value is zero, when no change is made. Conversion
-- to lowercase is similar, using the \lccode.
data ChangeCaseCode = NoCaseChange | ChangeToCode CharCode
  deriving stock (Show, Eq)

fmtChangeCaseCode :: Fmt ChangeCaseCode
fmtChangeCaseCode = F.later $ \case
  NoCaseChange -> F.bformat $ "No change"
  ChangeToCode c -> "Change to " <> F.bformat fmtCharCode c

instance HexCode ChangeCaseCode where
  toHexInt NoCaseChange = Q.HexInt 0
  toHexInt (ChangeToCode c) = toHexInt c

  fromHexInt h@(Q.HexInt n)
    | n < 0 = Nothing
    | n > 255 = Nothing
    | n == 0 = Just NoCaseChange
    | otherwise = ChangeToCode <$> fromHexInt h

newtype LowerCaseCode = LowerCaseCode ChangeCaseCode
  deriving stock (Generic)
  deriving newtype (Show, Eq, HexCode)

fmtLowerCaseCode :: Fmt LowerCaseCode
fmtLowerCaseCode = F.later $ \case
  LowerCaseCode c -> "Lowercase: " <> F.bformat fmtChangeCaseCode c

newtype UpperCaseCode = UpperCaseCode ChangeCaseCode
  deriving stock (Generic)
  deriving newtype (Show, Eq, HexCode)

fmtUpperCaseCode :: Fmt UpperCaseCode
fmtUpperCaseCode = F.later $ \case
  UpperCaseCode c -> "Uppercase: " <> F.bformat fmtChangeCaseCode c

-- Space factor code.
---------------------

newtype SpaceFactorCode = SpaceFactorCode {unSpaceFactorCode :: Q.HexInt}
  deriving stock (Show, Eq)

fmtSpaceFactorCode :: Fmt SpaceFactorCode
fmtSpaceFactorCode = "Space factor: " |%| F.accessed (.unSpaceFactorCode) Q.fmtHexInt

instance HexCode SpaceFactorCode where
  toHexInt (SpaceFactorCode n) = n

  fromHexInt (Q.HexInt n)
    | n < 0 = Nothing
    | n >= 0x8000 = Nothing
    | otherwise = Just $ SpaceFactorCode $ Q.HexInt n

-- Common.

data CodeType
  = CatCodeType
  | MathCodeType
  | UpperCaseCodeType
  | LowerCaseCodeType
  | SpaceFactorCodeType
  | DelimiterCodeType
  deriving stock (Show, Eq, Generic)

data CCodeType (c :: CodeType) where
  CCatCodeType :: CCodeType 'CatCodeType
  CMathCodeType :: CCodeType 'MathCodeType
  CUpperCaseCodeType :: CCodeType 'UpperCaseCodeType
  CLowerCaseCodeType :: CCodeType 'LowerCaseCodeType
  CSpaceFactorCodeType :: CCodeType 'SpaceFactorCodeType
  CDelimiterCodeType :: CCodeType 'DelimiterCodeType

deriving stock instance Show (CCodeType q)

deriving stock instance Eq (CCodeType q)
