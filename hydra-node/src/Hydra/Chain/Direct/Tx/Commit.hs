{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Tx.Commit where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Hydra.Cardano.Api.Network (networkIdToNetwork)
import Hydra.Chain (HeadId (..))
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.Tx.Helpers (UTxOWithScript, headIdToCurrencySymbol)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Initial as Initial
import Hydra.Ledger.Cardano (addReferenceInputs)
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  emptyTxBody,
  unsafeBuildTransaction,
 )
import Hydra.Party (Party, partyFromChain, partyToChain)
import Plutus.Orphans ()
import PlutusLedgerApi.V2 (CurrencySymbol)
import qualified PlutusLedgerApi.V2 as Plutus

-- | Craft a commit transaction which includes the "committed" utxo as a datum.
commitTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  HeadId ->
  Party ->
  -- | The UTxO to commit to the Head along with witnesses.
  UTxO' (TxOut CtxUTxO, Witness WitCtxTxIn) ->
  -- | The initial output (sent to each party) which should contain the PT and is
  -- locked by initial script
  (TxIn, TxOut CtxUTxO, Hash PaymentKey) ->
  Tx
commitTx networkId scriptRegistry headId party utxoToCommitWitnessed (initialInput, out, vkh) =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(initialInput, initialWitness)]
      & addReferenceInputs [initialScriptRef]
      & addInputs committedTxIns
      & addExtraRequiredSigners [vkh]
      & addOutputs [commitOutput]
 where
  initialWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference initialScriptRef initialScript initialDatum initialRedeemer

  initialScript =
    fromPlutusScript @PlutusScriptV2 Initial.validatorScript

  initialScriptRef =
    fst (initialReference scriptRegistry)

  initialDatum =
    mkScriptDatum $ Initial.datum (headIdToCurrencySymbol headId)

  initialRedeemer =
    toScriptData . Initial.redeemer $
      Initial.ViaCommit (toPlutusTxOutRef . fst <$> committedTxIns)

  committedTxIns =
    map (\(i, (_, w)) -> (i, BuildTxWith w)) $ UTxO.pairs utxoToCommitWitnessed

  commitOutput =
    TxOut commitAddress commitValue commitDatum ReferenceScriptNone

  commitScript =
    fromPlutusScript Commit.validatorScript

  commitAddress =
    mkScriptAddress @PlutusScriptV2 networkId commitScript

  commitValue =
    txOutValue out <> foldMap txOutValue utxoToCommit

  commitDatum =
    mkTxOutDatum $ mkCommitDatum party utxoToCommit (headIdToCurrencySymbol headId)

  utxoToCommit = fst <$> utxoToCommitWitnessed

mkCommitDatum :: Party -> UTxO -> CurrencySymbol -> Plutus.Datum
mkCommitDatum party utxo headId =
  Commit.datum (partyToChain party, commits, headId)
 where
  commits =
    mapMaybe Commit.serializeCommit $ UTxO.pairs utxo

data CommitObservation = CommitObservation
  { commitOutput :: UTxOWithScript
  , party :: Party
  , committed :: UTxO
  }

-- | Identify a commit tx by:
--
-- - Find which 'initial' tx input is being consumed,
-- - Find the redeemer corresponding to that 'initial', which contains the tx
--   input of the committed utxo,
-- - Find the outputs which pays to the commit validator,
-- - Using the datum of that output, deserialize the committed output,
-- - Reconstruct the committed UTxO from both values (tx input and output).
observeCommitTx ::
  NetworkId ->
  -- | Known (remaining) initial tx inputs.
  [TxIn] ->
  Tx ->
  Maybe CommitObservation
observeCommitTx networkId initials tx = do
  initialTxIn <- findInitialTxIn
  committedTxIns <- decodeInitialRedeemer initialTxIn

  (commitIn, commitOut) <- findTxOutByAddress commitAddress tx
  dat <- txOutScriptData commitOut
  (onChainParty, onChainCommits, _headId) :: Commit.DatumType <- fromScriptData dat
  party <- partyFromChain onChainParty

  committed <- do
    -- TODO: We could simplify this by just using the datum. However, we would
    -- need to ensure the commit is belonging to a head / is rightful. By just
    -- looking for some known initials we achieve this (a bit complicated) now.
    committedUTxO <- traverse (Commit.deserializeCommit (networkIdToNetwork networkId)) onChainCommits
    when (map fst committedUTxO /= committedTxIns) $
      error "TODO: commit redeemer not matching the serialized commits in commit datum"
    pure . UTxO.fromPairs $ committedUTxO

  pure
    CommitObservation
      { commitOutput = (commitIn, toUTxOContext commitOut, dat)
      , party
      , committed
      }
 where
  findInitialTxIn =
    case filter (`elem` initials) (txIns' tx) of
      [input] -> Just input
      _ -> Nothing

  decodeInitialRedeemer =
    findRedeemerSpending tx >=> \case
      Initial.ViaAbort ->
        Nothing
      Initial.ViaCommit{committedRefs} ->
        Just (fromPlutusTxOutRef <$> committedRefs)

  commitAddress = mkScriptAddress @PlutusScriptV2 networkId commitScript

  commitScript = fromPlutusScript Commit.validatorScript


