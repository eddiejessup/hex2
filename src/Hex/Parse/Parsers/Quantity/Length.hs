{-# LANGUAGE PatternSynonyms #-}

module Hex.Parse.Parsers.Quantity.Length where

import Control.Monad.Combinators qualified as PC
import Hex.Codes (pattern Chr_)
import Hex.Codes qualified as H.C
import Hex.Parse.Syntax.Quantity qualified as Syn
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators qualified as Par
import Hex.Parse.Parsers.Quantity.Number qualified as Par
import Hex.Quantity qualified as H.Q
import Hexlude

parseLength :: MonadPrimTokenSource m => m Syn.Length
parseLength = Par.parseSigned parseUnsignedLength

parseUnsignedLength :: MonadPrimTokenSource m => m Syn.UnsignedLength
parseUnsignedLength =
  PC.choice
    [ Syn.NormalLengthAsULength <$> parseNormalLength,
      Syn.CoercedLength . Syn.InternalGlueAsLength <$> Par.parseHeaded Par.headToParseInternalGlue
    ]

parseNormalLength :: MonadPrimTokenSource m => m Syn.NormalLength
parseNormalLength =
  PC.choice
    [ Syn.LengthSemiConstant <$> parseFactor <*> parseUnit,
      Syn.InternalLength <$> Par.parseHeaded Par.headToParseInternalLength
    ]

-- NOTE: The parser order matters because TeX's grammar is ambiguous: '2.2'
-- could be parsed as an integer constant, '2', followed by '.2'. We break the
-- ambiguity by prioritising the rational constant parser.
parseFactor :: MonadPrimTokenSource m => m Syn.Factor
parseFactor =
  PC.choice
    [ Syn.DecimalFractionFactor <$> parseRationalConstant,
      Syn.NormalIntFactor <$> Par.parseHeaded Par.headToParseNormalInt
    ]

parseRationalConstant :: MonadPrimTokenSource m => m Syn.DecimalFraction
parseRationalConstant = do
  wholeDigits <- PC.many (satisfyThen Par.decCharToWord)
  Par.skipSatisfied $ \t -> case t ^? Par.primTokCharCat of
    Just cc ->
      let chrCode = cc ^. typed @H.C.CharCode
       in (cc ^. typed @H.C.CoreCatCode == H.C.Other) && (chrCode == H.C.Chr_ ',' || chrCode == H.C.Chr_ '.')
    Nothing -> False
  fracDigits <- PC.many (satisfyThen Par.decCharToWord)
  pure $ Syn.DecimalFraction {Syn.wholeDigits, Syn.fracDigits}

parseUnit :: MonadPrimTokenSource m => m Syn.Unit
parseUnit =
  PC.choice
    [ Par.skipOptionalSpaces *> (Syn.InternalUnit <$> parseInternalUnit),
      (Syn.PhysicalUnit <$> parseFrame <*> parsePhysicalUnitLit) <* Par.skipOneOptionalSpace
    ]
  where
    parseInternalUnit =
      PC.choice
        [ parseInternalUnitLit <* Par.skipOneOptionalSpace,
          Syn.InternalIntUnit <$> Par.parseHeaded Par.headToParseInternalInt,
          Syn.InternalLengthUnit <$> Par.parseHeaded Par.headToParseInternalLength,
          Syn.InternalGlueUnit <$> Par.parseHeaded Par.headToParseInternalGlue
        ]

    parseInternalUnitLit =
      PC.choice
        [ Par.skipKeyword [Chr_ 'e', Chr_ 'm'] $> Syn.Em,
          Par.skipKeyword [Chr_ 'e', Chr_ 'x'] $> Syn.Ex
        ]

    parseFrame =
      Par.parseOptionalKeyword [Chr_ 't', Chr_ 'r', Chr_ 'u', Chr_ 'e'] <&> \case
        True -> Syn.TrueFrame
        False -> Syn.MagnifiedFrame

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
