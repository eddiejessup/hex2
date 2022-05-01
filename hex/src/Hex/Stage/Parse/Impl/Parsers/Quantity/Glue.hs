module Hex.Stage.Parse.Impl.Parsers.Quantity.Glue where

import Control.Monad.Combinators qualified as PC
import Hex.Common.Codes (pattern Chr_)
import Hex.Common.Codes qualified as H.C
import Hex.Stage.Parse.Interface.AST.Common qualified as AST
import Hex.Stage.Parse.Impl.Parsers.Combinators qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Length qualified as Par
import Hex.Stage.Parse.Impl.Parsers.Quantity.Number qualified as Par
import Hex.Common.Quantity qualified as H.Q
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as T
import Hexlude
import Hex.Stage.Expand.Interface (MonadPrimTokenSource)

headToParseModedAddGlue :: MonadPrimTokenSource m => H.Q.Axis -> T.PrimitiveToken -> m AST.Glue
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
        empty
  _ ->
    empty
  where
    noLengthGlueSpec = AST.ExplicitGlueSpec AST.zeroLength

parseGlue :: MonadPrimTokenSource m => m AST.Glue
parseGlue =
  PC.choice
    [ AST.ExplicitGlue <$> parseExplicitGlueSpec,
      AST.InternalGlue <$> Par.parseSigned (Par.parseHeaded Par.headToParseInternalGlue)
    ]

parseExplicitGlueSpec :: MonadPrimTokenSource m => m AST.ExplicitGlueSpec
parseExplicitGlueSpec = do
  len <- Par.parseLength
  stretch <- parseFlex [Chr_ 'p', Chr_ 'l', Chr_ 'u', Chr_ 's']
  shrink <- parseFlex [Chr_ 'm', Chr_ 'i', Chr_ 'n', Chr_ 'u', Chr_ 's']
  pure $ AST.ExplicitGlueSpec len stretch shrink

parseFlex :: MonadPrimTokenSource m => [H.C.CharCode] -> m (Maybe AST.Flex)
parseFlex s =
  PC.choice
    [ Just <$> parsePresentFlex,
      Par.skipOptionalSpaces $> Nothing
    ]
  where
    parsePresentFlex = do
      Par.skipKeyword s
      PC.choice
        [ AST.FiniteFlex <$> Par.parseLength,
          AST.FilFlex <$> parseFilLength
        ]

parseFilLength :: MonadPrimTokenSource m => m AST.FilLength
parseFilLength = do
  factor <- Par.parseSigned Par.parseFactor
  Par.skipKeyword [Chr_ 'f', Chr_ 'i']
  order <-
    parseNrLs >>= \case
      0 -> panic "impossible"
      1 -> pure H.Q.Fil1
      2 -> pure H.Q.Fil2
      3 -> pure H.Q.Fil3
      _ -> empty
  Par.skipOptionalSpaces
  pure $ AST.FilLength factor order
  where
    parseNrLs = length <$> PC.some (Par.skipSatisfied $ Par.matchNonActiveCharacterUncased (Chr_ 'l'))
