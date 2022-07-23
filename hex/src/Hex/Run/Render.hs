module Hex.Run.Render where

import Formatting qualified as F
import Hex.Capability.Log.Interface qualified as Log
import Hex.Common.DVI.DocInstruction qualified as DVI
import Hex.Common.DVI.SpecInstruction qualified as DVIS
import Hex.Common.DVI.SpecInstruction.Encode qualified as DVI.Enc
import Hex.Common.DVI.SpecInstruction.Types qualified as DVIS
import Hex.Common.HexInput.Interface (HexInput)
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Run.Paginate qualified as Paginate
import Hex.Stage.Build.ListExtractor.Interface (HexListExtractor)
import Hex.Stage.Evaluate.Interface (HexEvaluate)
import Hex.Stage.Interpret.AllMode (InterpretError)
import Hex.Stage.Parse.Interface (CommandSource)
import Hex.Stage.Render.DVI qualified as DVI
import Hexlude

renderToDocInstructions :: '[Error InterpretError, HexEvaluate, HexInput, CommandSource, HSt.EHexState, Log.HexLog, HexListExtractor] :>> es => Eff es [DVI.DocInstruction]
renderToDocInstructions = do
  pages <- Paginate.paginateAll
  pure $ DVI.pagesToDVI $ toList pages

fmtDocInstructions :: Fmt [DVI.DocInstruction]
fmtDocInstructions = F.unlined DVI.fmtDocInstruction

renderToSpecInstructions :: '[Error InterpretError, HexEvaluate, HexInput, CommandSource, HSt.EHexState, Log.HexLog, HexListExtractor] :>> es => Eff es [DVIS.SpecInstruction]
renderToSpecInstructions = do
  docInstrs <- renderToDocInstructions
  mag <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Mag)
  let (mayErr, _finalState, specInstrs) = DVIS.renderDocInstructions (DVIS.Magnification mag) docInstrs
  case mayErr of
    Nothing -> pure specInstrs
    Just dviError -> panic $ F.sformat DVIS.fmtDVIError dviError

fmtSpecInstructions :: Fmt [DVIS.SpecInstruction]
fmtSpecInstructions = F.unlined DVIS.fmtSpecInstruction

renderToDVIBytes :: [Error InterpretError, HexEvaluate, HexInput, CommandSource, HSt.EHexState, Log.HexLog, HexListExtractor] :>> es => Eff es ByteString
renderToDVIBytes = do
  specInstrs <- renderToSpecInstructions
  pure $ DVI.Enc.encodeSpecInstructions specInstrs
