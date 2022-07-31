module Hex.Stage.Parse.Impl.Parsers.Quantity.MathLength where

import Control.Monad.Combinators qualified as PC
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (PrimTokenSource (..))
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude

parseMathLength :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.MathLength
parseMathLength = AST.MathLength <$> Par.parseSigned parseUnsignedMathLength

parseUnsignedMathLength :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.UnsignedMathLength
parseUnsignedMathLength =
  PC.choice
    [ AST.NormalMathLengthAsUMathLength <$> parseNormalMathLength
    -- , AST.CoercedMathLength . AST.InternalMathGlueAsMathLength <$> parseHeaded headToParseInternalMathGlue
    ]

parseNormalMathLength :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.NormalMathLength
parseNormalMathLength =
  AST.MathLengthSemiConstant <$> Par.parseFactor <*> parseMathUnit

parseMathUnit :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.MathUnit
parseMathUnit =
  PC.choice
    [ skipKeyword PT.Expanding [Chr_ 'm', Chr_ 'u'] >> skipOneOptionalSpace PT.Expanding $> AST.Mu
    -- , skipOptionalSpaces *> (AST.InternalMathGlueAsUnit <$> parseHeaded headToParseInternalMathGlue)
    ]
