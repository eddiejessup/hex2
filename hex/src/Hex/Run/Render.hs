module Hex.Run.Render where

import Formatting qualified as F
import Hex.Common.DVI.DocInstruction qualified as DVI
import Hex.Common.DVI.SpecInstruction qualified as DVIS
import Hex.Common.DVI.SpecInstruction.Types qualified as DVIS
import Hex.Common.HexState.Interface qualified as HSt
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Run.App (App)
import Hex.Run.App qualified as App
import Hex.Run.Paginate qualified as Paginate
import Hex.Stage.Render.DVI qualified as DVI
import Hexlude
import qualified Hex.Common.DVI.SpecInstruction.Encode as DVI.Enc

renderToDocInstructions :: App [DVI.DocInstruction]
renderToDocInstructions = do
  pages <- Paginate.paginateAll
  pure $ DVI.pagesToDVI $ toList pages

fmtDocInstructions :: Fmt [DVI.DocInstruction]
fmtDocInstructions = F.unlined DVI.fmtDocInstruction

renderToSpecInstructions :: App [DVIS.SpecInstruction]
renderToSpecInstructions = do
  docInstrs <- renderToDocInstructions
  mag <- HSt.getParameterValue (HSt.Param.IntQuantParam HSt.Param.Mag)
  let (mayErr, _finalState, specInstrs) = DVIS.renderDocInstructions (DVIS.Magnification mag) docInstrs
  case mayErr of
    Nothing -> pure specInstrs
    Just (appError :: App.AppError) -> panic $ F.sformat App.fmtAppError appError

fmtSpecInstructions :: Fmt [DVIS.SpecInstruction]
fmtSpecInstructions = F.unlined DVIS.fmtSpecInstruction

renderToDVIBytes :: App ByteString
renderToDVIBytes = do
  specInstrs <- renderToSpecInstructions
  pure $ DVI.Enc.encodeSpecInstructions specInstrs

