module Hex.Run.Render where

import Formatting qualified as F
import Hex.Run.App (App)
import Hex.Run.Paginate qualified as Paginate
import Hex.Stage.Render.DVI qualified as DVI
import Hexlude

renderToDviInstructions :: App [DVI.DVIInstruction]
renderToDviInstructions = do
  pages <- Paginate.paginateAll
  pure $ DVI.pagesToDVI pages

fmtDviInstructions :: Fmt [DVI.DVIInstruction]
fmtDviInstructions = F.unlined DVI.fmtDviInstruction
