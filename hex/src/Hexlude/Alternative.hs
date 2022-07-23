{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Hexlude.Alternative where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))

data EAlternative :: Effect where
  AltEmpty :: EAlternative m a
  AltChoice :: m a -> m a -> EAlternative m a

makeEffect ''EAlternative

instance EAlternative :> es => Alternative (Eff es) where
    empty = send @EAlternative AltEmpty
    a <|> b = send @EAlternative (AltChoice a b)

data EMonadPlus :: Effect where
  MZero :: EMonadPlus m a
  MPlus :: m a -> m a -> EMonadPlus m a

makeEffect ''EMonadPlus

instance EAlternative :> es => MonadPlus (Eff es) where
