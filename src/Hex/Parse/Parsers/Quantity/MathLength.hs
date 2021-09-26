{-# LANGUAGE PatternSynonyms #-}

module Hex.Parse.Parsers.Quantity.MathLength where

import Control.Monad.Combinators qualified as PC
import Hex.Codes (pattern Chr_)
import Hex.Parse.Syntax.Quantity qualified as Syn
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators
import Hex.Parse.Parsers.Quantity.Length qualified as Par
import Hex.Parse.Parsers.Quantity.Number qualified as Par
import Hexlude

parseMathLength :: MonadPrimTokenSource m => m Syn.MathLength
parseMathLength = Par.parseSigned parseUnsignedMathLength

parseUnsignedMathLength :: MonadPrimTokenSource m => m Syn.UnsignedMathLength
parseUnsignedMathLength =
  PC.choice
    [ Syn.NormalMathLengthAsUMathLength <$> parseNormalMathLength
    -- , Syn.CoercedMathLength . Syn.InternalMathGlueAsMathLength <$> parseHeaded headToParseInternalMathGlue
    ]

parseNormalMathLength :: MonadPrimTokenSource m => m Syn.NormalMathLength
parseNormalMathLength =
  Syn.MathLengthSemiConstant <$> Par.parseFactor <*> parseMathUnit

parseMathUnit :: MonadPrimTokenSource m => m Syn.MathUnit
parseMathUnit =
  PC.choice
    [ skipKeyword [Chr_ 'm', Chr_ 'u'] >> skipOneOptionalSpace $> Syn.Mu
    -- , skipOptionalSpaces *> (Syn.InternalMathGlueAsUnit <$> parseHeaded headToParseInternalMathGlue)
    ]
