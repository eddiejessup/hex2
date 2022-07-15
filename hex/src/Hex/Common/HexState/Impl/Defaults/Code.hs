module Hex.Common.HexState.Impl.Defaults.Code where

import ASCII qualified
import ASCII.Predicates qualified as ASCII.Pred
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Impl.Defaults.Common (initialiseFiniteMap)
import Hex.Common.Quantity qualified as Q
import Hexlude

asciiPred :: (ASCII.Char -> Bool) -> Code.CharCode -> Bool
asciiPred f (Code.CharCode w) = maybe False f (ASCII.word8ToCharMaybe w)

newCatCodes :: Map Code.CharCode Code.CatCode
newCatCodes = initialiseFiniteMap $ \case
  Code.Chr_ '\\' -> Code.Escape
  Code.Chr_ ' ' -> Code.CoreCatCode Code.Space
  Code.Chr_ '%' -> Code.Comment
  Code.Chr_ '\r' -> Code.EndOfLine
  Code.Chr_ '\0' -> Code.Ignored
  Code.Chr_ '\DEL' -> Code.Invalid
  c | asciiPred ASCII.Pred.isLetter c -> Code.CoreCatCode Code.Letter
  _ -> Code.CoreCatCode Code.Other

-- All delcodes are −1 until they are changed by a \delcode command.
newDelimiterCodes :: Map Code.CharCode Code.DelimiterCode
newDelimiterCodes = initialiseFiniteMap $ const $ Code.NotADelimiter (Q.HexInt (-1))

-- The ten digits have \mathcode x = x + "7000.
-- The 52 letters have \mathcode x = x + "7100.
-- Otherwise,          \mathcode x = x
-- Put otherwise: letters are class 7, family 1; digits are class 7, family 0.
newMathCodes :: Map Code.CharCode Code.MathCode
newMathCodes = initialiseFiniteMap f
  where
    f c
      | asciiPred ASCII.Pred.isDigit c =
          Code.NormalMathCode Code.VariableFamily (Code.FamilyCharRef (Q.HexInt 0) c)
      | asciiPred ASCII.Pred.isLetter c =
          Code.NormalMathCode Code.VariableFamily (Code.FamilyCharRef (Q.HexInt 1) c)
      | otherwise =
          Code.NormalMathCode Code.Ordinary (Code.FamilyCharRef (Q.HexInt 0) c)

-- By default, all \uccode and \lccode values are zero except that the
-- letters a to z and A to Z have \uccode values A to Z and \lccode values a to
-- z.
newCaseCodes :: ASCII.Case -> Map Code.CharCode Code.ChangeCaseCode
newCaseCodes destCase = initialiseFiniteMap f
  where
    f c = case ASCII.word8ToCharMaybe c.unCharCode of
      Just asciiChar
        | ASCII.Pred.isLetter asciiChar ->
            Code.ChangeToCode $ Code.codeFromAsciiChar $ ASCII.toCaseChar destCase asciiChar
      _ -> Code.NoCaseChange

newLowercaseCodes :: Map Code.CharCode Code.LowerCaseCode
newLowercaseCodes = Code.LowerCaseCode <$> newCaseCodes ASCII.LowerCase

newUppercaseCodes :: Map Code.CharCode Code.UpperCaseCode
newUppercaseCodes = Code.UpperCaseCode <$> newCaseCodes ASCII.UpperCase

-- By default, all characters have a space factor code of 1000, except that the
-- uppercase letters ‘A’ through ‘Z’ have code 999.
newSpaceFactorCodes :: Map Code.CharCode Code.SpaceFactorCode
newSpaceFactorCodes = initialiseFiniteMap $ Code.SpaceFactorCode . f
  where
    f c
      | asciiPred ASCII.Pred.isUpper c = Q.HexInt 999
      | otherwise = Q.HexInt 1000
