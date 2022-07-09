module Hex.Stage.Parse.Impl.Parsers.Quantity.Glue where

import Control.Monad.Combinators qualified as PC
import Data.Foldable qualified as Fold
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hex.Common.Parse.Interface (MonadPrimTokenParse (..))
import Hex.Common.Parse.Interface qualified as Par
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Parse.Impl.Parsers.Combinators
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Stage.Parse.Interface.AST.Quantity qualified as AST
import Hexlude

headToParseModedAddGlue :: MonadPrimTokenParse m => Q.Axis -> T.PrimitiveToken -> m AST.Glue
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
        Par.parseFailure "headToParseModedAddGlue, ModedCommand"
  _ ->
    Par.parseFailure "headToParseModedAddGlue, no-ModedCommand"
  where
    noLengthGlueSpec = AST.ExplicitGlueSpec AST.zeroLength

parseGlue :: MonadPrimTokenParse m => m AST.Glue
parseGlue =
  PC.choice
    [ AST.ExplicitGlue <$> parseExplicitGlueSpec,
      AST.InternalGlue <$> Par.parseSigned (anyPrim >>= Par.headToParseInternalGlue)
    ]

parseExplicitGlueSpec :: MonadPrimTokenParse m => m AST.ExplicitGlueSpec
parseExplicitGlueSpec = do
  len <- Par.parseLength
  stretch <- parsePureFlex [Chr_ 'p', Chr_ 'l', Chr_ 'u', Chr_ 's']
  shrink <- parsePureFlex [Chr_ 'm', Chr_ 'i', Chr_ 'n', Chr_ 'u', Chr_ 's']
  pure $ AST.ExplicitGlueSpec len stretch shrink

parsePureFlex :: MonadPrimTokenParse m => [Code.CharCode] -> m (Maybe AST.PureFlex)
parsePureFlex s =
  PC.choice
    [ Just <$> parsePresentFlex,
      skipOptionalSpaces Expanding $> Nothing
    ]
  where
    parsePresentFlex = do
      skipKeyword Expanding s
      PC.choice
        [ AST.FinitePureFlex <$> Par.parseLength,
          AST.InfPureFlex <$> parseInfFlexOfOrder
        ]

parseInfFlexOfOrder :: MonadPrimTokenParse m => m AST.InfFlexOfOrder
parseInfFlexOfOrder = do
  factor <- Par.parseSigned Par.parseFactor
  skipKeyword Expanding [Chr_ 'f', Chr_ 'i']
  order <-
    parseNrLs >>= \case
      0 -> panic "impossible"
      1 -> pure Q.Fil1
      2 -> pure Q.Fil2
      3 -> pure Q.Fil3
      _ -> Par.parseFailure "parseInfFlexOfOrder"
  skipOptionalSpaces Expanding
  pure $ AST.InfFlexOfOrder factor order
  where
    parseNrLs =
      Fold.length <$> PC.some (skipSatisfied satisfyLexThenExpanding $ matchNonActiveCharacterUncased (Chr_ 'l'))
