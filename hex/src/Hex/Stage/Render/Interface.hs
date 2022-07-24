{-# LANGUAGE TemplateHaskell #-}

module Hex.Stage.Render.Interface where

import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.Vertical.Page.Types (Page)
import Hex.Stage.Render.Impl.DocInstruction.Encode qualified as Doc.Enc
import Hex.Stage.Render.Impl.SpecInstruction qualified as Spec
import Hex.Stage.Render.Impl.SpecInstruction.Encode qualified as Spec.Enc
import Hex.Stage.Render.Interface.DocInstruction (DocInstruction)
import Hex.Stage.Render.Interface.SpecInstruction (DVIError, Magnification, SpecInstruction)
import Hexlude

pagesToDocInstructions :: [Page] -> [DocInstruction]
pagesToDocInstructions = Doc.Enc.pagesToDocInstructions

pagesToSpecInstructions :: Magnification Q.HexInt -> [Page] -> Either DVIError [SpecInstruction]
pagesToSpecInstructions mag pages =
  let docInstrs = pagesToDocInstructions pages
      (mayErr, _st, instrs) = Spec.renderDocInstructions mag docInstrs
   in case mayErr of
        Nothing -> Right instrs
        Just err -> Left err

pagesToDVIBytes :: Magnification Q.HexInt -> [Page] -> Either DVIError ByteString
pagesToDVIBytes mag pages = do
  specInstrs <- pagesToSpecInstructions mag pages
  pure $ Spec.Enc.encodeSpecInstructions specInstrs
