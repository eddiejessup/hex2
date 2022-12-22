module Hex.Stage.Render.Interface where

import Hex.Common.HexState.Interface.Font qualified as HSt.Font
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.Vertical.Page.Types (Page)
import Hex.Stage.Render.Impl.DocInstruction.Encode qualified as Doc.Enc
import Hex.Stage.Render.Impl.SpecInstruction qualified as Spec
import Hex.Stage.Render.Impl.SpecInstruction.Encode qualified as Spec.Enc
import Hex.Stage.Render.Interface.SpecInstruction (DVIError, Magnification, SpecInstruction)
import Hexlude

pagesToSpecInstructions :: Magnification Q.HexInt -> [HSt.Font.FontDefinition] -> [Page] -> Either DVIError [SpecInstruction]
pagesToSpecInstructions mag fontDefinitions pages =
  let (docInstrs, usedFontNrs) = Doc.Enc.pagesToDocInstructions pages

      -- Only bother defining fonts that are actually used in the doc.
      usedFontDefinitions = filter (\def -> def.fontNr `elem` usedFontNrs) fontDefinitions

      (mayErr, _st, instrs) = Spec.renderDocInstructions mag usedFontDefinitions docInstrs
   in case mayErr of
        Nothing -> Right instrs
        Just err -> Left err

pagesToDVIBytes :: Magnification Q.HexInt -> [HSt.Font.FontDefinition] -> [Page] -> Either DVIError ByteString
pagesToDVIBytes mag fontDefinitions pages = do
  specInstrs <- pagesToSpecInstructions mag fontDefinitions pages
  pure $ Spec.Enc.encodeSpecInstructions specInstrs
