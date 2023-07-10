{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Tx.Fanout where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.Tx.Helpers (UTxOWithScript, headTokensFromValue)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.MintAction (MintAction (Burn))
import Hydra.Ledger.Cardano (addReferenceInputs)
import Hydra.Ledger.Cardano.Builder (
  addInputs,
  addOutputs,
  burnTokens,
  emptyTxBody,
  setValidityLowerBound,
  unsafeBuildTransaction,
 )
import Plutus.Orphans ()

-- | Create the fanout transaction, which distributes the closed state
-- accordingly. The head validator allows fanout only > deadline, so we need
-- to set the lower bound to be deadline + 1 slot.
fanoutTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Snapshotted UTxO to fanout on layer 1
  UTxO ->
  -- | Everything needed to spend the Head state-machine output.
  UTxOWithScript ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | Minting Policy script, made from initial seed
  PlutusScript ->
  Tx
fanoutTx scriptRegistry utxo (headInput, headOutput, ScriptDatumForTxIn -> headDatumBefore) deadlineSlotNo headTokenScript =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs orderedTxOutsToFanout
      & burnTokens headTokenScript Burn headTokens
      & setValidityLowerBound (deadlineSlotNo + 1)
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
    toScriptData (Head.Fanout $ fromIntegral $ length utxo)

  headTokens =
    headTokensFromValue headTokenScript (txOutValue headOutput)

  orderedTxOutsToFanout =
    toTxContext <$> toList utxo

data FanoutObservation = FanoutObservation

-- | Identify a fanout tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeFanoutTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe FanoutObservation
observeFanoutTx utxo tx = do
  headInput <- fst <$> findTxOutByScript @PlutusScriptV2 utxo headScript
  findRedeemerSpending tx headInput
    >>= \case
      Head.Fanout{} -> pure FanoutObservation
      _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript


