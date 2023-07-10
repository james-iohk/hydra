{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx.Abort where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Contract.MintAction (MintAction (Burn))
import Hydra.Ledger.Cardano (addReferenceInputs)
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  burnTokens,
  emptyTxBody,
  unsafeBuildTransaction,
 )
import Plutus.Orphans ()
import Hydra.Chain.Direct.Tx.Helpers (headTokensFromValue)

data AbortTxError = OverlappingInputs
  deriving (Show)

-- | Create transaction which aborts a head by spending the Head output and all
-- other "initial" outputs.
abortTx ::
  -- | Committed UTxOs to reimburse.
  UTxO ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO, HashableScriptData) ->
  -- | Script for monetary policy to burn tokens
  PlutusScript ->
  -- | Data needed to spend the initial output sent to each party to the Head.
  -- Should contain the PT and is locked by initial script.
  Map TxIn (TxOut CtxUTxO, HashableScriptData) ->
  -- | Data needed to spend commit outputs.
  -- Should contain the PT and is locked by commit script.
  Map TxIn (TxOut CtxUTxO, HashableScriptData) ->
  Either AbortTxError Tx
abortTx committedUTxO scriptRegistry vk (headInput, initialHeadOutput, ScriptDatumForTxIn -> headDatumBefore) headTokenScript initialsToAbort commitsToAbort
  | isJust (lookup headInput initialsToAbort) =
      Left OverlappingInputs
  | otherwise =
      Right $
        unsafeBuildTransaction $
          emptyTxBody
            & addInputs ((headInput, headWitness) : initialInputs <> commitInputs)
            & addReferenceInputs [initialScriptRef, commitScriptRef, headScriptRef]
            & addOutputs reimbursedOutputs
            & burnTokens headTokenScript Burn headTokens
            & addExtraRequiredSigners [verificationKeyHash vk]
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
    toScriptData Head.Abort

  initialInputs = mkAbortInitial <$> Map.toList initialsToAbort

  commitInputs = mkAbortCommit <$> Map.toList commitsToAbort

  headTokens =
    headTokensFromValue headTokenScript $
      mconcat
        [ txOutValue initialHeadOutput
        , foldMap (txOutValue . fst) initialsToAbort
        , foldMap (txOutValue . fst) commitsToAbort
        ]

  -- NOTE: Abort datums contain the datum of the spent state-machine input, but
  -- also, the datum of the created output which is necessary for the
  -- state-machine on-chain validator to control the correctness of the
  -- transition.
  mkAbortInitial (initialInput, (_, ScriptDatumForTxIn -> initialDatum)) =
    (initialInput, mkAbortInitialWitness initialDatum)

  mkAbortInitialWitness initialDatum =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference initialScriptRef initialScript initialDatum initialRedeemer
  initialScriptRef =
    fst (initialReference scriptRegistry)
  initialScript =
    fromPlutusScript @PlutusScriptV2 Initial.validatorScript
  initialRedeemer =
    toScriptData $ Initial.redeemer Initial.ViaAbort

  mkAbortCommit (commitInput, (_, ScriptDatumForTxIn -> commitDatum)) =
    (commitInput, mkAbortCommitWitness commitDatum)

  mkAbortCommitWitness commitDatum =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitScript commitDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitScript =
    fromPlutusScript @PlutusScriptV2 Commit.validatorScript
  commitRedeemer =
    toScriptData (Commit.redeemer Commit.ViaAbort)

  reimbursedOutputs = toTxContext . snd <$> UTxO.pairs committedUTxO

data AbortObservation = AbortObservation

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
-- FIXME: Add headId to AbortObservation to allow "upper layers" to
-- determine we are seeing an abort of "our head"
observeAbortTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe AbortObservation
observeAbortTx utxo tx = do
  headInput <- fst <$> findTxOutByScript @PlutusScriptV2 utxo headScript
  findRedeemerSpending tx headInput >>= \case
    Head.Abort -> pure AbortObservation
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript


