{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.Coin
  ( Coin (..),
    Core.CompactForm (..),
    word64ToCoin,
    coinToRational,
    rationalToCoinViaFloor,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Group (Abelian, Group (..))
import Data.Monoid (Sum (..))
import Data.PartialOrd (PartialOrd)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Quiet

-- | The amount of value held by a transaction output.
newtype Coin = Coin {unCoin :: Integer}
  deriving
    ( Eq,
      Ord,
      Enum,
      NoUnexpectedThunks,
      Generic,
      ToJSON,
      FromJSON,
      NFData
    )
  deriving (Show) via Quiet Coin
  deriving (ToCBOR, FromCBOR) via Core.Compact Coin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd)

word64ToCoin :: Word64 -> Coin
word64ToCoin w = Coin $ fromIntegral w

coinToRational :: Coin -> Rational
coinToRational (Coin c) = fromIntegral c

rationalToCoinViaFloor :: Rational -> Coin
rationalToCoinViaFloor r = Coin . floor $ r

-- FIXME:
-- if coin is less than 0 or greater than (maxBound :: Word64), then
-- fromIntegral constructs the incorrect value. for now this is handled
-- with an erroring bounds check here. where should this really live?
instance Core.Compactible Coin where
  newtype CompactForm Coin = CompactCoin Word64
  toCompact (Coin c)
    | c < 0 = error "out of bounds"
    | c > (fromIntegral (maxBound :: Word64)) =
      error "out of bounds"
    | otherwise = CompactCoin (fromIntegral c)
  fromCompact (CompactCoin c) = word64ToCoin c

instance ToCBOR (Core.CompactForm Coin) where
  toCBOR (CompactCoin c) = toCBOR c

instance FromCBOR (Core.CompactForm Coin) where
  fromCBOR = CompactCoin <$> fromCBOR
