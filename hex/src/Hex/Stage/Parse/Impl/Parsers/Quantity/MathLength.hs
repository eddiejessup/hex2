module Hex.Stage.Parse.Impl.Parsers.Quantity.MathLength where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Codes (pattern Chr_)
import Hex.Stage.Parse.Interface.AST.Common qualified as AST
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hexlude
import Hex.Stage.Expand.Interface (MonadPrimTokenSource)

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
