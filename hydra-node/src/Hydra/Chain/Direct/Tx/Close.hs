{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Tx.Close where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Chain (HeadId (..))
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Tx.Helpers (ClosedThreadOutput (..), OpenThreadOutput (..), UTxOHash (..), headIdToCurrencySymbol, findStateToken)
import Hydra.ContestationPeriod (ContestationPeriod, toChain)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import Hydra.Crypto (MultiSignature, toPlutusSignatures)
import Hydra.Data.ContestationPeriod (addContestationPeriod, posixFromUTCTime)
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (addReferenceInputs)
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  emptyTxBody,
  setValidityLowerBound,
  setValidityUpperBound,
  unsafeBuildTransaction,
 )
import Hydra.Snapshot (Snapshot (..), SnapshotNumber, fromChainSnapshot)
import Plutus.Orphans ()
import PlutusLedgerApi.V2 (toBuiltin)
import qualified PlutusLedgerApi.V2 as Plutus

-- | Low-level data type of a snapshot to close the head with. This is different
-- to the 'ConfirmedSnasphot', which is provided to `CloseTx` as it also
-- contains relevant chain state like the 'openUtxoHash'.
data ClosingSnapshot
  = CloseWithInitialSnapshot {openUtxoHash :: UTxOHash}
  | CloseWithConfirmedSnapshot
      { snapshotNumber :: SnapshotNumber
      , closeUtxoHash :: UTxOHash
      , -- XXX: This is a bit of a wart and stems from the fact that our
        -- SignableRepresentation of 'Snapshot' is in fact the snapshotNumber
        -- and the closeUtxoHash as also included above
        signatures :: MultiSignature (Snapshot Tx)
      }

-- | Create a transaction closing a head with either the initial snapshot or
-- with a multi-signed confirmed snapshot.
closeTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | The snapshot to close with, can be either initial or confirmed one.
  ClosingSnapshot ->
  -- | Lower validity slot number, usually a current or quite recent slot number.
  SlotNo ->
  -- | Upper validity slot and UTC time to compute the contestation deadline time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  OpenThreadOutput ->
  -- | Head identifier
  HeadId ->
  Tx
closeTx scriptRegistry vk closing startSlotNo (endSlotNo, utcTime) openThreadOutput headId =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs [headOutputAfter]
      & addExtraRequiredSigners [verificationKeyHash vk]
      & setValidityLowerBound startSlotNo
      & setValidityUpperBound endSlotNo
 where
  OpenThreadOutput
    { openThreadUTxO = (headInput, headOutputBefore, ScriptDatumForTxIn -> headDatumBefore)
    , openContestationPeriod
    , openParties
    } = openThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript headDatumBefore headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript

  headRedeemer =
    toScriptData
      Head.Close
        { signature
        }

  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  headDatumAfter =
    mkTxOutDatum
      Head.Closed
        { snapshotNumber
        , utxoHash = toBuiltin utxoHashBytes
        , parties = openParties
        , contestationDeadline
        , contestationPeriod = openContestationPeriod
        , headId = headIdToCurrencySymbol headId
        , contesters = []
        }

  snapshotNumber = toInteger $ case closing of
    CloseWithInitialSnapshot{} -> 0
    CloseWithConfirmedSnapshot{snapshotNumber = sn} -> sn

  UTxOHash utxoHashBytes = case closing of
    CloseWithInitialSnapshot{openUtxoHash} -> openUtxoHash
    CloseWithConfirmedSnapshot{closeUtxoHash} -> closeUtxoHash

  signature = case closing of
    CloseWithInitialSnapshot{} -> mempty
    CloseWithConfirmedSnapshot{signatures = s} -> toPlutusSignatures s

  contestationDeadline =
    addContestationPeriod (posixFromUTCTime utcTime) openContestationPeriod

-- XXX: This function is VERY similar to the 'closeTx' function (only notable
-- difference being the redeemer, which is in itself also the same structure as
-- the close's one. We could potentially refactor this to avoid repetition or do
-- something more principled at the protocol level itself and "merge" close and
-- contest as one operation.
contestTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Contested snapshot number (i.e. the one we contest to)
  Snapshot Tx ->
  -- | Multi-signature of the whole snapshot
  MultiSignature (Snapshot Tx) ->
  -- | Current slot and posix time to be used as the contestation time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  ClosedThreadOutput ->
  HeadId ->
  ContestationPeriod ->
  Tx
contestTx
  scriptRegistry
  vk
  Snapshot{number, utxo}
  sig
  (slotNo, _)
  ClosedThreadOutput
    { closedThreadUTxO =
      (headInput, headOutputBefore, ScriptDatumForTxIn -> headDatumBefore)
    , closedParties
    , closedContestationDeadline
    , closedContesters
    }
  headId
  contestationPeriod =
    unsafeBuildTransaction $
      emptyTxBody
        & addInputs [(headInput, headWitness)]
        & addReferenceInputs [headScriptRef]
        & addOutputs [headOutputAfter]
        & addExtraRequiredSigners [verificationKeyHash vk]
        & setValidityUpperBound slotNo
   where
    headWitness =
      BuildTxWith $
        ScriptWitness scriptWitnessInCtx $
          mkScriptReference headScriptRef headScript headDatumBefore headRedeemer
    headScriptRef =
      fst (headReference scriptRegistry)
    headScript =
      fromPlutusScript @PlutusScriptV2 Head.validatorScript
    headRedeemer =
      toScriptData
        Head.Contest
          { signature = toPlutusSignatures sig
          }
    headOutputAfter =
      modifyTxOutDatum (const headDatumAfter) headOutputBefore

    contester = toPlutusKeyHash (verificationKeyHash vk)

    onChainConstestationPeriod = toChain contestationPeriod

    newContestationDeadline =
      if length (contester : closedContesters) == length closedParties
        then closedContestationDeadline
        else addContestationPeriod closedContestationDeadline onChainConstestationPeriod

    headDatumAfter =
      mkTxOutDatum
        Head.Closed
          { snapshotNumber = toInteger number
          , utxoHash
          , parties = closedParties
          , contestationDeadline = newContestationDeadline
          , contestationPeriod = onChainConstestationPeriod
          , headId = headIdToCurrencySymbol headId
          , contesters = contester : closedContesters
          }
    utxoHash = toBuiltin $ hashUTxO @Tx utxo

data CloseObservation = CloseObservation
  { threadOutput :: ClosedThreadOutput
  , headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  }
  deriving (Show, Eq)

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CloseObservation
observeCloseTx utxo tx = do
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 utxo headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- lookupScriptData tx headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open{parties}, Head.Close{}) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- lookupScriptData tx newHeadOutput
      (closeContestationDeadline, onChainSnapshotNumber) <- case fromScriptData newHeadDatum of
        Just Head.Closed{contestationDeadline, snapshotNumber} -> pure (contestationDeadline, snapshotNumber)
        _ -> Nothing
      pure
        CloseObservation
          { threadOutput =
              ClosedThreadOutput
                { closedThreadUTxO =
                    ( newHeadInput
                    , newHeadOutput
                    , newHeadDatum
                    )
                , closedParties = parties
                , closedContestationDeadline = closeContestationDeadline
                , closedContesters = []
                }
          , headId
          , snapshotNumber = fromChainSnapshot onChainSnapshotNumber
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

data ContestObservation = ContestObservation
  { contestedThreadOutput :: (TxIn, TxOut CtxUTxO, HashableScriptData)
  , headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contesters :: [Plutus.PubKeyHash]
  }
  deriving (Show, Eq)

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeContestTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe ContestObservation
observeContestTx utxo tx = do
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 utxo headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- lookupScriptData tx headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Closed{contesters}, Head.Contest{}) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- lookupScriptData tx newHeadOutput
      let onChainSnapshotNumber = closedSnapshotNumber newHeadDatum
      pure
        ContestObservation
          { contestedThreadOutput =
              ( newHeadInput
              , newHeadOutput
              , newHeadDatum
              )
          , headId
          , snapshotNumber = fromChainSnapshot onChainSnapshotNumber
          , contesters
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

  closedSnapshotNumber headDatum =
    case fromScriptData headDatum of
      Just Head.Closed{snapshotNumber} -> snapshotNumber
      _ -> error "wrong state in output datum"


