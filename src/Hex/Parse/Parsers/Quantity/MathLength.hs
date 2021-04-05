{-# LANGUAGE PatternSynonyms #-}

module Hex.Parse.Parsers.Quantity.MathLength where

import Control.Monad.Combinators qualified as PC
import Hex.Codes (pattern Chr_)
import Hex.Parse.AST qualified as AST
import Hex.Parse.MonadPrimTokenSource.Interface
import Hex.Parse.Parsers.Combinators
import Hex.Parse.Parsers.Quantity.Number qualified as Par
import Hex.Parse.Parsers.Quantity.Length qualified as Par
import Hexlude

parseMathLength :: MonadPrimTokenSource m => m AST.MathLength
parseMathLength = Par.parseSigned parseUnsignedMathLength

parseUnsignedMathLength :: MonadPrimTokenSource m => m AST.UnsignedMathLength
parseUnsignedMathLength =
  PC.choice
    [ AST.NormalMathLengthAsUMathLength <$> parseNormalMathLength
    -- , AST.CoercedMathLength . AST.InternalMathGlueAsMathLength <$> parseHeaded headToParseInternalMathGlue
    ]

parseNormalMathLength :: MonadPrimTokenSource m => m AST.NormalMathLength
parseNormalMathLength =
  AST.MathLengthSemiConstant <$> Par.parseFactor <*> parseMathUnit

parseMathUnit :: MonadPrimTokenSource m => m AST.MathUnit
parseMathUnit =
  PC.choice
    [ skipKeyword [Chr_ 'm', Chr_ 'u'] >> skipOneOptionalSpace $> AST.Mu
    -- , skipOptionalSpaces *> (AST.InternalMathGlueAsUnit <$> parseHeaded headToParseInternalMathGlue)
    ]
