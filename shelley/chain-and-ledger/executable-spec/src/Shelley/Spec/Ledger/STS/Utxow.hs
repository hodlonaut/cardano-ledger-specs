{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Utxow
  ( UTXOW,
    UtxowPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.Era (Era)
import Cardano.Prelude (NoUnexpectedThunks (..), asks)
import Control.Iterate.SetAlgebra (eval, (∩))
import Control.Monad (when)
import Control.State.Transition
  ( Embed,
    IRC (..),
    InitialRule,
    STS (..),
    TRC (..),
    TransitionRule,
    failBecause,
    judgmentContext,
    liftSTS,
    trans,
    wrapFailed,
    (?!),
    (?!:),
  )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
  ( ShelleyBase,
    StrictMaybe (..),
    invalidKey,
    quorum,
    (==>),
  )
import Shelley.Spec.Ledger.Delegation.Certificates (isInstantaneousRewards)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KeyHash,
    KeyRole (..),
    VKey,
    asWitness,
  )
import Shelley.Spec.Ledger.LedgerState
  ( UTxOState (..),
    WitHashes (..),
    diffWitHashes,
    nullWitHashes,
    verifiedWits,
    witsFromWitnessSet,
    witsVKeyNeeded,
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash, hashMetaData, validMetaData)
import Shelley.Spec.Ledger.STS.Utxo (UTXO, UtxoEnv (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization (decodeList, decodeRecordSum, decodeSet, encodeFoldable)
import qualified Shelley.Spec.Ledger.SoftForks as SoftForks
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    hashScript,
    txwitsScript,
    validateScript,
  )
import Shelley.Spec.Ledger.TxBody (TxBody (..))
import Shelley.Spec.Ledger.UTxO (scriptsNeeded)

data UTXOW era

data UtxowPredicateFailure era
  = InvalidWitnessesUTXOW
      ![VKey 'Witness era]
  | -- witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      !(WitHashes era) -- witnesses which were needed and not supplied
  | MissingScriptWitnessesUTXOW
      !(Set (ScriptHash era)) -- missing scripts
  | ScriptWitnessNotValidatingUTXOW
      !(Set (ScriptHash era)) -- failed scripts
  | UtxoFailure (PredicateFailure (UTXO era))
  | MIRInsufficientGenesisSigsUTXOW (Set (KeyHash 'Witness era))
  | MissingTxBodyMetaDataHash
      !(MetaDataHash era) -- hash of the full metadata
  | MissingTxMetaData
      !(MetaDataHash era) -- hash of the metadata included in the transaction body
  | ConflictingMetaDataHash
      !(MetaDataHash era) -- hash of the metadata included in the transaction body
      !(MetaDataHash era) -- hash of the full metadata
      -- Contains out of range values (strings too long)
  | InvalidMetaData
  deriving (Eq, Generic, Show)

instance (Era era) => NoUnexpectedThunks (UtxowPredicateFailure era)

instance
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  STS (UTXOW era)
  where
  type State (UTXOW era) = UTxOState era
  type Signal (UTXOW era) = Tx era
  type Environment (UTXOW era) = UtxoEnv era
  type BaseM (UTXOW era) = ShelleyBase
  type PredicateFailure (UTXOW era) = UtxowPredicateFailure era
  transitionRules = [utxoWitnessed]
  initialRules = [initialLedgerStateUTXOW]

instance
  (Typeable era, Era era) =>
  ToCBOR (UtxowPredicateFailure era)
  where
  toCBOR = \case
    InvalidWitnessesUTXOW wits ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> encodeFoldable wits
    MissingVKeyWitnessesUTXOW (WitHashes missing) ->
      encodeListLen 2 <> toCBOR (1 :: Word8) <> encodeFoldable missing
    MissingScriptWitnessesUTXOW ss -> encodeListLen 2 <> toCBOR (2 :: Word8) <> encodeFoldable ss
    ScriptWitnessNotValidatingUTXOW ss -> encodeListLen 2 <> toCBOR (3 :: Word8) <> encodeFoldable ss
    (UtxoFailure a) ->
      encodeListLen 2 <> toCBOR (4 :: Word8)
        <> toCBOR a
    MIRInsufficientGenesisSigsUTXOW sigs -> encodeListLen 2 <> toCBOR (5 :: Word8) <> encodeFoldable sigs
    MissingTxBodyMetaDataHash h ->
      encodeListLen 2 <> toCBOR (6 :: Word8) <> toCBOR h
    MissingTxMetaData h ->
      encodeListLen 2 <> toCBOR (7 :: Word8) <> toCBOR h
    ConflictingMetaDataHash bodyHash fullMDHash ->
      encodeListLen 3 <> toCBOR (8 :: Word8) <> toCBOR bodyHash <> toCBOR fullMDHash
    InvalidMetaData ->
      encodeListLen 1 <> toCBOR (9 :: Word8)

instance
  (Era era) =>
  FromCBOR (UtxowPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (UTXOW era)" $
    \case
      0 -> do
        wits <- decodeList fromCBOR
        pure (2, InvalidWitnessesUTXOW wits)
      1 -> do
        missing <- decodeSet fromCBOR
        pure (2, MissingVKeyWitnessesUTXOW $ WitHashes missing)
      2 -> do
        ss <- decodeSet fromCBOR
        pure (2, MissingScriptWitnessesUTXOW ss)
      3 -> do
        ss <- decodeSet fromCBOR
        pure (2, ScriptWitnessNotValidatingUTXOW ss)
      4 -> do
        a <- fromCBOR
        pure (2, UtxoFailure a)
      5 -> do
        s <- decodeSet fromCBOR
        pure (2, MIRInsufficientGenesisSigsUTXOW s)
      6 -> do
        h <- fromCBOR
        pure (2, MissingTxBodyMetaDataHash h)
      7 -> do
        h <- fromCBOR
        pure (2, MissingTxMetaData h)
      8 -> do
        bodyHash <- fromCBOR
        fullMDHash <- fromCBOR
        pure (3, ConflictingMetaDataHash bodyHash fullMDHash)
      9 -> pure (1, InvalidMetaData)
      k -> invalidKey k

initialLedgerStateUTXOW ::
  forall era.
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  InitialRule (UTXOW era)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp stakepools genDelegs) <- judgmentContext
  trans @(UTXO era) $ IRC (UtxoEnv slots pp stakepools genDelegs)

utxoWitnessed ::
  forall era.
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  TransitionRule (UTXOW era)
utxoWitnessed =
  judgmentContext
    >>= \(TRC (UtxoEnv slot pp stakepools genDelegs, u, tx@(Tx txbody wits md))) -> do
      let utxo = _utxo u
      let witsKeyHashes = witsFromWitnessSet wits

      -- check multi-signature scripts
      let failedScripts =
            filter
              (\(hs, validator) -> hashScript validator /= hs || not (validateScript validator tx))
              (Map.toList $ txwitsScript tx)
      case failedScripts of
        [] -> pure ()
        fs -> failBecause $ ScriptWitnessNotValidatingUTXOW $ Set.fromList $ fmap fst fs

      let sNeeded = scriptsNeeded utxo tx
          sReceived = Map.keysSet (txwitsScript tx)
      sNeeded == sReceived ?! MissingScriptWitnessesUTXOW (sNeeded `Set.difference` sReceived)

      -- check VKey witnesses
      verifiedWits tx ?!: InvalidWitnessesUTXOW

      let needed = witsVKeyNeeded utxo tx genDelegs
          missingWitnesses = diffWitHashes needed witsKeyHashes
          haveNeededWitnesses = case nullWitHashes missingWitnesses of
            True -> Right ()
            False -> Left missingWitnesses
      haveNeededWitnesses ?!: MissingVKeyWitnessesUTXOW

      -- check metadata hash
      case (_mdHash txbody, md) of
        (SNothing, SNothing) -> pure ()
        (SJust mdh, SNothing) -> failBecause $ MissingTxMetaData mdh
        (SNothing, SJust md') -> failBecause $ MissingTxBodyMetaDataHash (hashMetaData md')
        (SJust mdh, SJust md') -> do
          hashMetaData md' == mdh ?! ConflictingMetaDataHash mdh (hashMetaData md')
          -- check metadata value sizes
          when (SoftForks.validMetaData pp) $ validMetaData md' ?! InvalidMetaData

      -- check genesis keys signatures for instantaneous rewards certificates
      let genDelegates = Set.fromList $ fmap (asWitness . genDelegKeyHash) $ Map.elems genMapping
          (WitHashes khAsSet) = witsKeyHashes
          genSig = eval (genDelegates ∩ khAsSet)
          mirCerts =
            StrictSeq.toStrict
              . Seq.filter isInstantaneousRewards
              . StrictSeq.getSeq
              $ _certs txbody
          GenDelegs genMapping = genDelegs

      coreNodeQuorum <- liftSTS $ asks quorum
      ( (not $ null mirCerts)
          ==> Set.size genSig >= fromIntegral coreNodeQuorum
        )
        ?! MIRInsufficientGenesisSigsUTXOW genSig

      trans @(UTXO era) $
        TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)

instance
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  Embed (UTXO era) (UTXOW era)
  where
  wrapFailed = UtxoFailure
