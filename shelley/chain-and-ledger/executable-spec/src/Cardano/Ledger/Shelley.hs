{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Core (Compactible (..), Value)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era
import Cardano.Ledger.Val (Val)
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.Coin (Coin)

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

data Shelley c

instance CryptoClass.Crypto c => Era (Shelley c) where
  type Crypto (Shelley c) = c

type instance Value (Shelley c) = Coin

type ShelleyEra era =
  ( Era era,
    Val (Value era),
    Compactible (Value era),
    Eq (Value era),
    FromCBOR (CompactForm (Value era)),
    FromCBOR (Value era),
    NFData (Value era),
    NoUnexpectedThunks (Value era),
    Show (Value era),
    ToCBOR (CompactForm (Value era)),
    ToCBOR (Value era),
    Typeable (Value era)
  )
