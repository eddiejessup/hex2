module Hex.Stage.Parse.Impl.Parsers.Quantity.MathGlue where

import Control.Monad.Combinators qualified as PC
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as Code
import Hex.Common.Token.Resolved.Primitive qualified as PT
import Hex.Stage.Expand.Interface (PrimTokenSource (..), parseFail)
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Glue qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.MathLength qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hex.Stage.Parse.Interface.AST.Quantity qualified as Par
import Hexlude

parseMathGlue :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es Par.MathGlue
parseMathGlue =
  PC.choice
    [ AST.ExplicitMathGlue
        <$> Par.parseMathLength
        <*> parsePureMathFlex [Chr_ 'p', Chr_ 'l', Chr_ 'u', Chr_ 's']
        <*> parsePureMathFlex [Chr_ 'm', Chr_ 'i', Chr_ 'n', Chr_ 'u', Chr_ 's'],
      AST.InternalMathGlue <$> Par.parseSigned (anyPrim >>= headToParseInternalMathGlue)
    ]

parsePureMathFlex :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => [Code.CharCode] -> Eff es (Maybe AST.PureMathFlex)
parsePureMathFlex s =
  PC.choice
    [ Just <$> parsePresentFlex,
      skipOptionalSpaces PT.Expanding $> Nothing
    ]
  where
    parsePresentFlex =
      skipKeyword PT.Expanding s
        *> PC.choice
          [ AST.FinitePureMathFlex <$> Par.parseMathLength,
            AST.InfPureMathFlex <$> Par.parseInfFlexOfOrder
          ]

headToParseInternalMathGlue :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => PT.PrimitiveToken -> Eff es AST.InternalMathGlue
headToParseInternalMathGlue =
  choiceFlap
    [ fmap AST.InternalMathGlueVariable <$> Par.headToParseMathGlueVariable,
      \case
        PT.LastGlueTok ->
          pure AST.LastMathGlue
        t ->
          parseFail $ "headToParseInternalMathGlue " <> F.sformat PT.fmtPrimitiveToken t
    ]
