module Hex.Run.Render where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Run.Paginate qualified as Paginate
import Hex.Stage.Build.ListExtractor.Interface (ExtractList)
import Hex.Stage.Evaluate.Interface (HexEvaluate)
import Hex.Stage.Interpret.AllMode (InterpretError)
import Hex.Stage.Parse.Interface (CommandSource)
import Hex.Stage.Read.Interface (HexInput)
import Hex.Stage.Render.Interface qualified as Render
import Hex.Stage.Render.Interface.DocInstruction qualified as Render.Doc
import Hex.Stage.Render.Interface.SpecInstruction qualified as Render.Spec
import Hexlude

renderToDocInstructions :: '[Error InterpretError, HexEvaluate, HexInput, CommandSource, HSt.EHexState, Log.HexLog, ExtractList] :>> es => Eff es [Render.Doc.DocInstruction]
renderToDocInstructions = do
  pages <- Paginate.paginateAll
  pure $ Render.pagesToDocInstructions $ toList pages

renderToSpecInstructions :: '[Error InterpretError, HexEvaluate, HexInput, CommandSource, HSt.EHexState, Log.HexLog, ExtractList] :>> es => Eff es [Render.Spec.SpecInstruction]
renderToSpecInstructions = do
  pages <- Paginate.paginateAll
  mag <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Mag)
  case Render.pagesToSpecInstructions (Render.Spec.Magnification mag) (toList pages) of
    Left err -> panic $ F.sformat Render.Spec.fmtDVIError err
    Right specInstrs -> pure specInstrs

renderToDVIBytes :: [Error InterpretError, HexEvaluate, HexInput, CommandSource, HSt.EHexState, Log.HexLog, ExtractList] :>> es => Eff es ByteString
renderToDVIBytes = do
  pages <- Paginate.paginateAll
  mag <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Mag)
  case Render.pagesToDVIBytes (Render.Spec.Magnification mag) (toList pages) of
    Left err -> panic $ F.sformat Render.Spec.fmtDVIError err
    Right specInstrs -> pure specInstrs
