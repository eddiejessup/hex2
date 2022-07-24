module Hex.Stage.Parse.Impl.Parsers.Quantity.Glue where

import Control.Monad.Combinators qualified as PC
import Data.List qualified as List
import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as Code
import Hex.Common.Quantity qualified as Q
import Hex.Common.Token.Resolved.Primitive qualified as T
import Hex.Stage.Expand.Interface (PrimTokenSource (..))
import Hex.Stage.Expand.Interface qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude

headToParseModedAddGlue :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Axis -> T.PrimitiveToken -> Eff es AST.Glue
headToParseModedAddGlue axis = \case
  T.ModedCommand tokenAxis modedTok | tokenAxis == axis ->
    case modedTok of
      -- \hskip 10pt and such.
      T.SpecifiedGlueTok ->
        parseGlue
      T.PresetGlueTok presetTok ->
        pure $
          AST.ExplicitGlue $ case presetTok of
            -- \{v,h}fil:    0pt plus 1fil
            T.Fil ->
              noLengthGlueSpec (Just AST.oneFilFlex) Nothing
            -- \{v,h}fill:   0pt plus 1fill
            T.Fill ->
              noLengthGlueSpec (Just AST.oneFillFlex) Nothing
            -- \{v,h}ss:     0pt plus 1fil minus 1fil
            T.StretchOrShrink ->
              noLengthGlueSpec (Just AST.oneFilFlex) (Just AST.oneFilFlex)
            -- \{v,h}filneg: 0pt plus -1fil
            T.FilNeg ->
              noLengthGlueSpec (Just AST.minusOneFilFlex) Nothing
      _ ->
        Par.parseFail "headToParseModedAddGlue, ModedCommand"
  _ ->
    Par.parseFail "headToParseModedAddGlue, no-ModedCommand"
  where
    noLengthGlueSpec = AST.ExplicitGlueSpec AST.zeroLength

parseGlue :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.Glue
parseGlue =
  PC.choice
    [ AST.ExplicitGlue <$> parseExplicitGlueSpec,
      Par.tryParse $ AST.InternalGlue <$> Par.parseSigned (anyPrim >>= Par.headToParseInternalGlue)
    ]

parseExplicitGlueSpec :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.ExplicitGlueSpec
parseExplicitGlueSpec = do
  len <- Par.parseLength
  stretch <- parsePureFlex [Chr_ 'p', Chr_ 'l', Chr_ 'u', Chr_ 's']
  shrink <- parsePureFlex [Chr_ 'm', Chr_ 'i', Chr_ 'n', Chr_ 'u', Chr_ 's']
  pure $ AST.ExplicitGlueSpec len stretch shrink

parsePureFlex :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => [Code.CharCode] -> Eff es (Maybe AST.PureFlex)
parsePureFlex s =
  PC.choice
    [ Just <$> parsePresentFlex,
      skipOptionalSpaces Expanding $> Nothing
    ]
  where
    parsePresentFlex = do
      skipKeyword Expanding s
      PC.choice
        [ Par.tryParse $ AST.FinitePureFlex <$> Par.parseLength,
          AST.InfPureFlex <$> parseInfFlexOfOrder
        ]

parseInfFlexOfOrder :: [PrimTokenSource, EAlternative, Log.HexLog] :>> es => Eff es AST.InfFlexOfOrder
parseInfFlexOfOrder = do
  factor <- Par.parseSigned Par.parseFactor
  skipKeyword Expanding [Chr_ 'f', Chr_ 'i']
  order <-
    parseNrLs >>= \case
      0 -> panic "impossible"
      1 -> pure Q.Fil1
      2 -> pure Q.Fil2
      3 -> pure Q.Fil3
      n -> Par.parseFail $ "parseInfFlexOfOrder " <> F.sformat F.int n
  skipOptionalSpaces Expanding
  pure $ AST.InfFlexOfOrder factor order
  where
    parseNrLs =
      List.length <$> PC.some (skipSatisfied (satisfyCharCatThen Expanding) $ matchNonActiveCharacterUncased (Chr_ 'l'))
