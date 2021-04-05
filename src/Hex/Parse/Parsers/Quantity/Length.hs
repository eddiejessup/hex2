{-# LANGUAGE PatternSynonyms #-}

module Hex.Parse.Parsers.Quantity.Length where

import Control.Monad.Combinators qualified as PC
import Hex.Codes (pattern Chr_)
import Hex.Codes qualified as H.C
import Hex.Parse.AST qualified as AST
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators qualified as Par
import Hex.Parse.Parsers.Quantity.Number qualified as Par
import Hex.Quantity qualified as H.Q
import Hexlude

parseLength :: MonadPrimTokenSource m => m AST.Length
parseLength = Par.parseSigned parseUnsignedLength

parseUnsignedLength :: MonadPrimTokenSource m => m AST.UnsignedLength
parseUnsignedLength =
  PC.choice
    [ AST.NormalLengthAsULength <$> parseNormalLength,
      AST.CoercedLength . AST.InternalGlueAsLength <$> Par.parseHeaded Par.headToParseInternalGlue
    ]

parseNormalLength :: MonadPrimTokenSource m => m AST.NormalLength
parseNormalLength =
  PC.choice
    [ AST.LengthSemiConstant <$> parseFactor <*> parseUnit,
      AST.InternalLength <$> Par.parseHeaded Par.headToParseInternalLength
    ]

-- NOTE: The parser order matters because TeX's grammar is ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
parseFactor :: MonadPrimTokenSource m => m AST.Factor
parseFactor =
  PC.choice
    [ AST.DecimalFractionFactor <$> parseRationalConstant,
      AST.NormalIntFactor <$> Par.parseHeaded Par.headToParseNormalInt
    ]

parseRationalConstant :: MonadPrimTokenSource m => m AST.DecimalFraction
parseRationalConstant = do
  wholeDigits <- PC.many (satisfyThen Par.decCharToWord)
  Par.skipSatisfied $ \t -> case t ^? Par.primTokCharCat of
    Just cc ->
      let chrCode = cc ^. typed @H.C.CharCode
       in (cc ^. typed @H.C.CoreCatCode == H.C.Other) && (chrCode == H.C.Chr_ ',' || chrCode == H.C.Chr_ '.')
    Nothing -> False
  fracDigits <- PC.many (satisfyThen Par.decCharToWord)
  pure $ AST.DecimalFraction {AST.wholeDigits, AST.fracDigits}

parseUnit :: MonadPrimTokenSource m => m AST.Unit
parseUnit =
  PC.choice
    [ Par.skipOptionalSpaces *> (AST.InternalUnit <$> parseInternalUnit),
      (AST.PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit) <* Par.skipOneOptionalSpace
    ]
  where
    parseInternalUnit =
      PC.choice
        [ parseInternalUnitLit <* Par.skipOneOptionalSpace,
          AST.InternalIntUnit <$> Par.parseHeaded Par.headToParseInternalInt,
          AST.InternalLengthUnit <$> Par.parseHeaded Par.headToParseInternalLength,
          AST.InternalGlueUnit <$> Par.parseHeaded Par.headToParseInternalGlue
        ]

    parseInternalUnitLit =
      PC.choice
        [ Par.skipKeyword [Chr_ 'e', Chr_ 'm'] $> AST.Em,
          Par.skipKeyword [Chr_ 'e', Chr_ 'x'] $> AST.Ex
        ]

    parseFrame =
      Par.parseOptionalKeyword [Chr_ 't', Chr_ 'r', Chr_ 'u', Chr_ 'e'] <&> \case
        True -> AST.TrueFrame
        False -> AST.MagnifiedFrame

    -- TODO: Use 'try' because keywords with common prefixes lead the parser
    -- down a blind alley. Could refactor to avoid, but it would be ugly.
    -- Leave as later optimisation.
    -- TODO: Should we omit the last try in such cases?
    -- NOTE: Can't trim number of 'try's naïvely, because they all suck up
    -- initial space, which would also need backtracking.
    parsePhysicalUnitLit =
      PC.choice
        [ Par.skipKeyword [Chr_ 'b', Chr_ 'p'] $> H.Q.BigPoint,
          Par.skipKeyword [Chr_ 'c', Chr_ 'c'] $> H.Q.Cicero,
          Par.skipKeyword [Chr_ 'c', Chr_ 'm'] $> H.Q.Centimetre,
          Par.skipKeyword [Chr_ 'd', Chr_ 'd'] $> H.Q.Didot,
          Par.skipKeyword [Chr_ 'i', Chr_ 'n'] $> H.Q.Inch,
          Par.skipKeyword [Chr_ 'm', Chr_ 'm'] $> H.Q.Millimetre,
          Par.skipKeyword [Chr_ 'p', Chr_ 'c'] $> H.Q.Pica,
          Par.skipKeyword [Chr_ 'p', Chr_ 't'] $> H.Q.Point,
          Par.skipKeyword [Chr_ 's', Chr_ 'p'] $> H.Q.ScaledPoint
        ]
