{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Hex.Codes where

import Protolude

import ASCII qualified
import ASCII.Predicates qualified as ASCII.Pred
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Path (File, Path)
import Path qualified
import Hex.Interpret.Evaluate.Evaluated (HexInt(..))

class HexCode a where
    toHexInt :: a -> HexInt

    fromHexInt :: HexInt -> Maybe a

newtype CharCode = CharCode { codeWord :: Word8 }
    deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Hashable)

pattern CharCode_ :: Char -> CharCode
pattern CharCode_ c <- (unsafeCodeAsChar -> c)
  where
    CharCode_ c = unsafeCodeFromChar c

unsafeCodeAsChar :: CharCode -> Char
unsafeCodeAsChar (CharCode w) = case ASCII.word8ToCharMaybe w of
  Just asciiChar -> ASCII.charToUnicode asciiChar
  Nothing -> chr $ fromIntegral w

unsafeCodeFromChar :: Char -> CharCode
unsafeCodeFromChar c = case ASCII.unicodeToCharMaybe c of
  Just asciiChar -> CharCode $ ASCII.charToWord8 asciiChar
  Nothing -> panic $ show c

codeFromAsciiChar :: ASCII.Char -> CharCode
codeFromAsciiChar = CharCode . ASCII.charToWord8

codeInt :: CharCode -> Int
codeInt = fromIntegral . codeWord

asciiPred :: (ASCII.Char -> Bool) -> CharCode -> Bool
asciiPred f (CharCode w) = maybe False f (ASCII.word8ToCharMaybe w)

instance HexCode CharCode where
    toHexInt = HexInt . fromIntegral

    fromHexInt n
        | n > 256 = Nothing
        | n < 0 = Nothing
        | otherwise = Just $ fromIntegral n

readCharCodes :: MonadIO m => Path a File -> m BS.ByteString
readCharCodes = liftIO . BS.readFile . Path.toFilePath

initialiseCharCodeMap :: (CharCode -> v) -> Map CharCode v
initialiseCharCodeMap val = Map.fromList $
    (\c -> (c, val c)) <$> [minBound..maxBound]

-- Category code
-----------------

data CatCode
    = Escape       -- 0
    | EndOfLine    -- 5
    | Ignored      -- 9
    | Comment      -- 14
    | Invalid      -- 15
    | CoreCatCode CoreCatCode
    deriving stock (Show, Eq, Generic)

instance HexCode CatCode where
    toHexInt = \case
        Escape                  -> 0
        CoreCatCode BeginGroup  -> 1
        CoreCatCode EndGroup    -> 2
        CoreCatCode MathShift   -> 3
        CoreCatCode AlignTab    -> 4
        EndOfLine               -> 5
        CoreCatCode Parameter   -> 6
        CoreCatCode Superscript -> 7
        CoreCatCode Subscript   -> 8
        Ignored                 -> 9
        CoreCatCode Space       -> 10
        CoreCatCode Letter      -> 11
        CoreCatCode Other       -> 12
        CoreCatCode Active      -> 13
        Comment                 -> 14
        Invalid                 -> 15

    fromHexInt = \case
        0  -> Just Escape
        1  -> Just $ CoreCatCode BeginGroup
        2  -> Just $ CoreCatCode EndGroup
        3  -> Just $ CoreCatCode MathShift
        4  -> Just $ CoreCatCode AlignTab
        5  -> Just EndOfLine
        6  -> Just $ CoreCatCode Parameter
        7  -> Just $ CoreCatCode Superscript
        8  -> Just $ CoreCatCode Subscript
        9  -> Just Ignored
        10 -> Just $ CoreCatCode Space
        11 -> Just $ CoreCatCode Letter
        12 -> Just $ CoreCatCode Other
        13 -> Just $ CoreCatCode Active
        14 -> Just Comment
        15 -> Just Invalid
        _ -> Nothing

newCatCodes :: Map CharCode CatCode
newCatCodes = initialiseCharCodeMap $ \case
    CharCode_ '\\'    -> Escape
    CharCode_ ' '     -> CoreCatCode Space
    CharCode_ '%'     -> Comment
    CharCode_ '\n'    -> EndOfLine  -- Non-Standard.
    CharCode_ '\r'    -> EndOfLine
    CharCode_ '\0'    -> Ignored
    CharCode_ '\DEL'  -> Invalid
    c | asciiPred ASCII.Pred.isLetter c -> CoreCatCode Letter
    _ -> CoreCatCode Other

-- Add useful extras beyond the technical defaults.
usableCatCodes :: Map CharCode CatCode
usableCatCodes = foldl' (\m (k, v) -> Map.insert k v m) newCatCodes extras
  where
    extras =
      [ (CharCode_ '^', CoreCatCode Superscript)
      , (CharCode_ '{', CoreCatCode BeginGroup)
      , (CharCode_ '}', CoreCatCode EndGroup)
      , (CharCode_ '#', CoreCatCode Parameter)
      ]

catLookup :: Map CharCode CatCode -> CharCode -> CatCode
catLookup m n = Map.findWithDefault Invalid n m

usableCatLookup :: CharCode -> CatCode
usableCatLookup = catLookup usableCatCodes

-- Not all Catcodes make it past the lexer.
data CoreCatCode
    = BeginGroup   -- 1
    | EndGroup     -- 2
    | MathShift    -- 3
    | AlignTab     -- 4
    | Parameter    -- 6
    | Superscript  -- 7
    | Subscript    -- 8
    | Space        -- 10
    | Letter       -- 11
    | Other        -- 12
    | Active       -- 13
    deriving stock (Show, Eq, Generic)

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
data DelimiterCode = NotADelimiter HexInt | DelimiterSpecCode DelimiterSpec
    deriving stock (Show)

instance HexCode DelimiterCode where
    toHexInt (NotADelimiter n) = n
    toHexInt (DelimiterSpecCode DelimiterSpec{smallVar, largeVar}) =
        (toHexInt largeVar `shiftL` 12) + toHexInt smallVar

    fromHexInt n
        | n < 0 = Just $ NotADelimiter n
        | n > 0xFFFFFF = Nothing
        | otherwise =
            do
            smallVar <- fromHexInt $ n `shiftR` 12
            largeVar <- fromHexInt $ n .&. 0xFFF
            Just $ DelimiterSpecCode $ DelimiterSpec smallVar largeVar

data DelimiterSpec = DelimiterSpec { smallVar, largeVar :: DelimiterVar }
    deriving stock (Show)

data DelimiterVar = PresentDelimiterVar FamilyCharRef | NullDelimiterVar
    deriving stock (Show)

instance HexCode DelimiterVar where
    toHexInt NullDelimiterVar        = 0
    toHexInt (PresentDelimiterVar f) = toHexInt f

    fromHexInt n
        | n == 0 = Just NullDelimiterVar
        | otherwise = PresentDelimiterVar <$> fromHexInt n

data FamilyCharRef = FamilyCharRef { family :: HexInt, position :: CharCode }
    deriving stock (Show)

instance HexCode FamilyCharRef where
    toHexInt (FamilyCharRef fam pos) = (fam `shiftL` 8) + fromIntegral pos

    fromHexInt n
        | n < 0 = Nothing
        | n > 0xFFF = Nothing
        | otherwise = Just $ FamilyCharRef (n `shiftR` 8) (fromIntegral (n .&. 0xFF))

-- All delcodes are −1 until they are changed by a \delcode command.
newDelimiterCodes :: Map CharCode DelimiterCode
newDelimiterCodes = initialiseCharCodeMap $ const $ NotADelimiter (-1)

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
    deriving stock (Show)

instance HexCode MathCode where
    toHexInt ActiveMathCode =
        0x8000
    toHexInt (NormalMathCode cls famRef) =
        toHexInt famRef + (toHexInt cls `shiftL` 12)

    fromHexInt n
        | n < 0 = Nothing
        | n > 0x8000 = Nothing
        | n == 0x8000 = Just ActiveMathCode
        | otherwise =
            NormalMathCode <$> fromHexInt (n `shiftR` 12) <*> fromHexInt (n .&. 0xFFF)

data MathClass =
      Ordinary        -- 0
    | LargeOperator   -- 1
    | BinaryRelation  -- 2
    | Relation        -- 3
    | Opening         -- 4
    | Closing         -- 5
    | Punctuation     -- 6
    | VariableFamily  -- 7
    deriving stock ( Show, Enum, Bounded )

instance HexCode MathClass where
    toHexInt = HexInt . fromEnum

    fromHexInt = Just . toEnum . unInt

-- The ten digits have \mathcode x = x + "7000.
-- The 52 letters have \mathcode x = x + "7100.
-- Otherwise,          \mathcode x = x
-- Put otherwise: letters are class 7, family 1; digits are class 7, family 0.
newMathCodes :: Map CharCode MathCode
newMathCodes = initialiseCharCodeMap f
  where
    f c
        | asciiPred ASCII.Pred.isDigit c =
            NormalMathCode VariableFamily (FamilyCharRef (HexInt 0) c)
        | asciiPred ASCII.Pred.isLetter c =
            NormalMathCode VariableFamily (FamilyCharRef (HexInt 1) c)
        | otherwise =
            NormalMathCode Ordinary       (FamilyCharRef (HexInt 0) c)

-- Change case code.
-- -----------------

-- Conversion to uppercase means that a character is replaced by its \uccode
-- value, unless the \uccode value is zero, when no change is made. Conversion
-- to lowercase is similar, using the \lccode.
data CaseChangeCode = NoCaseChange | ChangeToCode CharCode
    deriving stock (Show)

instance HexCode CaseChangeCode where
    toHexInt NoCaseChange     = 0
    toHexInt (ChangeToCode c) = toHexInt c

    fromHexInt n
        | n < 0 = Nothing
        | n > 255 = Nothing
        | n == 0 = Just NoCaseChange
        | otherwise = ChangeToCode <$> fromHexInt n

-- By default, all \uccode and \lccode values are zero except that the
-- letters a to z and A to Z have \uccode values A to Z and \lccode values a to
-- z.
newCaseCodes :: ASCII.Case -> Map CharCode CaseChangeCode
newCaseCodes destCase = initialiseCharCodeMap f
  where
    f c = case ASCII.word8ToCharMaybe (codeWord c) of
        Just asciiChar | ASCII.Pred.isLetter asciiChar ->
            ChangeToCode $ codeFromAsciiChar $ ASCII.toCaseChar destCase asciiChar
        _ -> NoCaseChange

newLowercaseCodes :: Map CharCode CaseChangeCode
newLowercaseCodes = newCaseCodes ASCII.LowerCase

newUppercaseCodes :: Map CharCode CaseChangeCode
newUppercaseCodes = newCaseCodes ASCII.UpperCase

-- Space factor code.
---------------------

newtype SpaceFactorCode = SpaceFactorCode HexInt
    deriving stock (Show)

instance HexCode SpaceFactorCode where
    toHexInt (SpaceFactorCode n) = n

    fromHexInt n
        | n < 0 = Nothing
        | n >= 0x8000 = Nothing
        | otherwise = Just $ SpaceFactorCode n

-- By default, all characters have a space factor code of 1000, except that the
-- uppercase letters ‘A’ through ‘Z’ have code 999.
newSpaceFactors :: Map CharCode SpaceFactorCode
newSpaceFactors = initialiseCharCodeMap $ SpaceFactorCode . f
  where
    f c
        | asciiPred ASCII.Pred.isUpper c = 999
        | otherwise = 1000
