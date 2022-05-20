module Hex.Stage.Parse.Impl.Parsers.Quantity.MathLength where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude

parseMathLength :: MonadPrimTokenParse m => m AST.MathLength
parseMathLength = AST.MathLength <$> Par.parseSigned parseUnsignedMathLength

parseUnsignedMathLength :: MonadPrimTokenParse m => m AST.UnsignedMathLength
parseUnsignedMathLength =
  PC.choice
    [ AST.NormalMathLengthAsUMathLength <$> parseNormalMathLength
    -- , AST.CoercedMathLength . AST.InternalMathGlueAsMathLength <$> parseHeaded headToParseInternalMathGlue
    ]

parseNormalMathLength :: MonadPrimTokenParse m => m AST.NormalMathLength
parseNormalMathLength =
  AST.MathLengthSemiConstant <$> Par.parseFactor <*> parseMathUnit

parseMathUnit :: MonadPrimTokenParse m => m AST.MathUnit
parseMathUnit =
  PC.choice
    [ skipKeyword Expanding [Chr_ 'm', Chr_ 'u'] >> skipOneOptionalSpace Expanding $> AST.Mu
    -- , skipOptionalSpaces *> (AST.InternalMathGlueAsUnit <$> parseHeaded headToParseInternalMathGlue)
    ]
