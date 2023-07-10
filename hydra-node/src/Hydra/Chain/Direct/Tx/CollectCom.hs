{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Tx.CollectCom where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Data.Map as Map
import Hydra.Chain (HeadId (..))
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.Tx.Helpers (
  InitialThreadOutput (..),
  OpenThreadOutput (..),
  UTxOHash (..), findStateToken, headIdToCurrencySymbol,
 )
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import Hydra.Ledger.Cardano (addReferenceInputs)
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  emptyTxBody,
  unsafeBuildTransaction,
 )
import Plutus.Orphans ()
import PlutusLedgerApi.V2 (fromBuiltin)

-- | Create a transaction collecting all "committed" utxo and opening a Head,
-- i.e. driving the Head script state.
collectComTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Everything needed to spend the Head state-machine output.
  InitialThreadOutput ->
  -- | Data needed to spend the commit output produced by each party.
  -- Should contain the PT and is locked by @ν_commit@ script.
  Map TxIn (TxOut CtxUTxO, HashableScriptData) ->
  -- | Head id
  HeadId ->
  Tx
collectComTx networkId scriptRegistry vk initialThreadOutput commits headId =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs ((headInput, headWitness) : (mkCommit <$> Map.toList commits))
      & addReferenceInputs [commitScriptRef, headScriptRef]
      & addOutputs [headOutput]
      & addExtraRequiredSigners [verificationKeyHash vk]
 where
  InitialThreadOutput
    { initialThreadUTxO =
      (headInput, initialHeadOutput, ScriptDatumForTxIn -> headDatumBefore)
    , initialParties
    , initialContestationPeriod
    } = initialThreadOutput
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript headDatumBefore headRedeemer
  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headScriptRef = fst (headReference scriptRegistry)
  headRedeemer = toScriptData Head.CollectCom
  headOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV2 networkId headScript)
      (txOutValue initialHeadOutput <> commitValue)
      headDatumAfter
      ReferenceScriptNone
  headDatumAfter =
    mkTxOutDatum
      Head.Open
        { Head.parties = initialParties
        , utxoHash
        , contestationPeriod = initialContestationPeriod
        , headId = headIdToCurrencySymbol headId
        }

  extractCommits d =
    case fromScriptData d of
      Nothing -> error "SNAFU"
      Just ((_, cs, _) :: Commit.DatumType) -> cs

  utxoHash =
    Head.hashPreSerializedCommits $ foldMap (extractCommits . snd . snd) $ Map.toList commits

  mkCommit (commitInput, (_commitOutput, commitDatum)) =
    ( commitInput
    , mkCommitWitness commitDatum
    )
  mkCommitWitness (ScriptDatumForTxIn -> commitDatum) =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitScript commitDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitValue =
    mconcat $ txOutValue . fst <$> Map.elems commits
  commitScript =
    fromPlutusScript @PlutusScriptV2 Commit.validatorScript
  commitRedeemer =
    toScriptData $ Commit.redeemer Commit.ViaCollectCom

data CollectComObservation = CollectComObservation
  { threadOutput :: OpenThreadOutput
  , headId :: HeadId
  , utxoHash :: UTxOHash
  }
  deriving (Show, Eq)

-- | Identify a collectCom tx by lookup up the input spending the Head output
-- and decoding its redeemer.
observeCollectComTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CollectComObservation
observeCollectComTx utxo tx = do
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 utxo headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- lookupScriptData tx headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Initial{parties, contestationPeriod}, Head.CollectCom) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- lookupScriptData tx newHeadOutput
      utxoHash <- UTxOHash <$> decodeUtxoHash newHeadDatum
      pure
        CollectComObservation
          { threadOutput =
              OpenThreadOutput
                { openThreadUTxO =
                    ( newHeadInput
                    , newHeadOutput
                    , newHeadDatum
                    )
                , openParties = parties
                , openContestationPeriod = contestationPeriod
                }
          , headId
          , utxoHash
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript
  decodeUtxoHash datum =
    case fromScriptData datum of
      Just Head.Open{utxoHash} -> Just $ fromBuiltin utxoHash
      _ -> Nothing


