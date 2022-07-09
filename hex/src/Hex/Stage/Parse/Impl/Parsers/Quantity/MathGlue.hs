module Hex.Stage.Parse.Impl.Parsers.Quantity.MathGlue where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..), parseFailure)
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Glue qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.MathLength qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hex.Stage.Parse.Interface.AST.Quantity qualified as Par
import Hexlude

parseMathGlue :: MonadPrimTokenParse m => m Par.MathGlue
parseMathGlue =
  PC.choice
    [ AST.ExplicitMathGlue
        <$> Par.parseMathLength
        <*> parsePureMathFlex [Chr_ 'p', Chr_ 'l', Chr_ 'u', Chr_ 's']
        <*> parsePureMathFlex [Chr_ 'm', Chr_ 'i', Chr_ 'n', Chr_ 'u', Chr_ 's'],
      AST.InternalMathGlue <$> Par.parseSigned (anyPrim >>= headToParseInternalMathGlue)
    ]

parsePureMathFlex :: MonadPrimTokenParse m => [Code.CharCode] -> m (Maybe AST.PureMathFlex)
parsePureMathFlex s =
  PC.choice
    [ Just <$> parsePresentFlex,
      skipOptionalSpaces Expanding $> Nothing
    ]
  where
    parsePresentFlex =
      skipKeyword Expanding s
        *> PC.choice
          [ AST.FinitePureMathFlex <$> Par.parseMathLength,
            AST.InfPureMathFlex <$> Par.parseInfFlexOfOrder
          ]

headToParseInternalMathGlue :: MonadPrimTokenParse m => PT.PrimitiveToken -> m AST.InternalMathGlue
headToParseInternalMathGlue =
  choiceFlap
    [ fmap AST.InternalMathGlueVariable <$> Par.headToParseMathGlueVariable,
      \case
        PT.LastGlueTok ->
          pure AST.LastMathGlue
        _ ->
          parseFailure "headToParseInternalMathGlue"
    ]
