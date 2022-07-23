module Hex.Stage.Parse.Impl.Parsers.Quantity.MathLength where

import Control.Monad.Combinators qualified as PC
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Parse.Interface (PrimTokenParse (..))
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude

parseMathLength :: [PrimTokenParse, EAlternative, Log.HexLog] :>> es => Eff es AST.MathLength
parseMathLength = AST.MathLength <$> Par.parseSigned parseUnsignedMathLength

parseUnsignedMathLength :: [PrimTokenParse, EAlternative, Log.HexLog] :>> es => Eff es AST.UnsignedMathLength
parseUnsignedMathLength =
  PC.choice
    [ AST.NormalMathLengthAsUMathLength <$> parseNormalMathLength
    -- , AST.CoercedMathLength . AST.InternalMathGlueAsMathLength <$> parseHeaded headToParseInternalMathGlue
    ]

parseNormalMathLength :: [PrimTokenParse, EAlternative, Log.HexLog] :>> es => Eff es AST.NormalMathLength
parseNormalMathLength =
  AST.MathLengthSemiConstant <$> Par.parseFactor <*> parseMathUnit

parseMathUnit :: [PrimTokenParse, EAlternative, Log.HexLog] :>> es => Eff es AST.MathUnit
parseMathUnit =
  PC.choice
    [ skipKeyword Expanding [Chr_ 'm', Chr_ 'u'] >> skipOneOptionalSpace Expanding $> AST.Mu
    -- , skipOptionalSpaces *> (AST.InternalMathGlueAsUnit <$> parseHeaded headToParseInternalMathGlue)
    ]
