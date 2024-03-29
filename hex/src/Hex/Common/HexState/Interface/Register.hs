module Hex.Common.HexState.Interface.Register where

import Formatting qualified as F
import Hex.Common.Quantity qualified as Q
import Hexlude

newtype RegisterLocation = RegisterLocation {unRegisterLocation :: Q.HexInt}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

allRegisterLocations :: [RegisterLocation]
allRegisterLocations = RegisterLocation . Q.HexInt <$> [0 .. 255]

instance Bounded RegisterLocation where
  minBound = RegisterLocation $ Q.HexInt 0

  maxBound = RegisterLocation $ Q.HexInt 255

data QuantRegisterLocation (q :: Q.QuantityType)
  = QuantRegisterLocation (QuantRegisterType q) RegisterLocation

data QuantRegisterType (q :: Q.QuantityType) where
  IntQuantRegisterType :: QuantRegisterType 'Q.IntQuantity
  LengthQuantRegisterType :: QuantRegisterType 'Q.LengthQuantity
  GlueQuantRegisterType :: QuantRegisterType 'Q.GlueQuantity
  MathGlueQuantRegisterType :: QuantRegisterType 'Q.MathGlueQuantity
  TokenListQuantRegisterType :: QuantRegisterType 'Q.TokenListQuantity

deriving stock instance Show (QuantRegisterType q)

deriving stock instance Eq (QuantRegisterType q)

deriving stock instance Show (QuantRegisterLocation q)

deriving stock instance Eq (QuantRegisterLocation q)

fmtRegisterLocation :: Fmt RegisterLocation
fmtRegisterLocation = F.squared (F.accessed (.unRegisterLocation) Q.fmtHexInt)

data BoxFetchMode
  = Pop
  | Lookup
  deriving stock (Show, Eq, Generic)
