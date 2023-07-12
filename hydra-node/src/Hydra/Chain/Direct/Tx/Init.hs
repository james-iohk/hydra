{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Tx.Init where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Data.Set as Set
import Hydra.Chain (HeadId (..), HeadParameters (..))
import Hydra.Chain.Direct.Tx.Helpers (
  InitialThreadOutput (..),
  UTxOWithScript,
  assetNameFromVerificationKey,
  headValue,
  hydraHeadV1AssetName, findFirst, mkHeadId,
 )
import Hydra.ContestationPeriod (ContestationPeriod, fromChain, toChain)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.HeadTokens as HeadTokens
import qualified Hydra.Contract.Initial as Initial
import Hydra.Contract.MintAction (MintAction (Mint))
import Hydra.Ledger.Cardano.Builder (
  addOutputs,
  addVkInputs,
  emptyTxBody,
  mintTokens,
  unsafeBuildTransaction,
 )
import Hydra.Party (Party, partyFromChain, partyToChain)
import Plutus.Orphans ()

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx ::
  NetworkId ->
  -- | All participants cardano keys.
  [VerificationKey PaymentKey] ->
  HeadParameters ->
  TxIn ->
  Tx
initTx networkId cardanoKeys parameters seedTxIn =
  unsafeBuildTransaction $
    emptyTxBody
      & addVkInputs [seedTxIn]
      & addOutputs
        ( mkHeadOutputInitial networkId seedTxIn parameters
            : map (mkInitialOutput networkId seedTxIn) cardanoKeys
        )
      & mintTokens (HeadTokens.mkHeadTokenScript seedTxIn) Mint ((hydraHeadV1AssetName, 1) : participationTokens)
 where
  participationTokens =
    [(assetNameFromVerificationKey vk, 1) | vk <- cardanoKeys]

mkHeadOutput :: NetworkId -> PolicyId -> TxOutDatum ctx -> TxOut ctx
mkHeadOutput networkId tokenPolicyId datum =
  TxOut
    (mkScriptAddress @PlutusScriptV2 networkId headScript)
    (headValue <> valueFromList [(AssetId tokenPolicyId hydraHeadV1AssetName, 1)])
    datum
    ReferenceScriptNone
 where
  headScript = fromPlutusScript Head.validatorScript

mkHeadOutputInitial :: NetworkId -> TxIn -> HeadParameters -> TxOut CtxTx
mkHeadOutputInitial networkId seedTxIn HeadParameters{contestationPeriod, parties} =
  mkHeadOutput networkId tokenPolicyId headDatum
 where
  tokenPolicyId = HeadTokens.headPolicyId seedTxIn
  headDatum =
    mkTxOutDatum $
      Head.Initial
        { contestationPeriod = toChain contestationPeriod
        , parties = map partyToChain parties
        , headId = toPlutusCurrencySymbol tokenPolicyId
        , seed = toPlutusTxOutRef seedTxIn
        }

mkInitialOutput :: NetworkId -> TxIn -> VerificationKey PaymentKey -> TxOut CtxTx
mkInitialOutput networkId seedTxIn (verificationKeyHash -> pkh) =
  TxOut initialAddress initialValue initialDatum ReferenceScriptNone
 where
  tokenPolicyId = HeadTokens.headPolicyId seedTxIn
  initialValue =
    headValue <> valueFromList [(AssetId tokenPolicyId (AssetName $ serialiseToRawBytes pkh), 1)]
  initialAddress =
    mkScriptAddress @PlutusScriptV2 networkId initialScript
  initialScript =
    fromPlutusScript Initial.validatorScript
  initialDatum =
    mkTxOutDatum $ Initial.datum (toPlutusCurrencySymbol tokenPolicyId)

data InitObservation = InitObservation
  { threadOutput :: InitialThreadOutput
  -- ^ The state machine UTxO produced by the Init transaction
  -- This output should always be present and 'threaded' across all
  -- transactions.
  -- NOTE(SN): The Head's identifier is somewhat encoded in the TxOut's address
  -- XXX(SN): Data and [OnChain.Party] are overlapping
  , initials :: [UTxOWithScript]
  , commits :: [UTxOWithScript]
  , headId :: HeadId
  , seedTxIn :: TxIn
  , contestationPeriod :: ContestationPeriod
  , parties :: [Party]
  }
  deriving (Show, Eq)

data NotAnInitReason
  = NotAHeadPolicy
  | NoHeadOutput
  | NotAHeadDatum
  | NoSTFound
  | PartiesMismatch
  | OwnPartyMissing
  | CPMismatch
  | PTsNotMintedCorrectly
  deriving (Show, Eq)

observeInitTx ::
  NetworkId ->
  [VerificationKey PaymentKey] ->
  -- | Our node's contestation period
  ContestationPeriod ->
  Party ->
  [Party] ->
  Tx ->
  Either NotAnInitReason InitObservation
observeInitTx networkId cardanoKeys expectedCP party otherParties tx = do
  -- XXX: Lots of redundant information here
  (ix, headOut, headData, headState) <-
    maybeLeft NoHeadOutput $
      findFirst headOutput indexedOutputs

  -- check that we have a proper head
  (headId, contestationPeriod, onChainParties, seedTxIn) <- case headState of
    (Head.Initial cp ps cid outRef) -> do
      pure (fromPlutusCurrencySymbol cid, fromChain cp, ps, fromPlutusTxOutRef outRef)
    _ -> Left NotAHeadDatum

  let offChainParties = concatMap partyFromChain onChainParties
  let stQuantity = selectAsset (txOutValue headOut) (AssetId headId hydraHeadV1AssetName)

  -- check that ST is present in the head output
  unless (stQuantity == 1) $
    Left NoSTFound

  -- check that we are using the same seed and headId matches
  unless (headId == HeadTokens.headPolicyId seedTxIn) $
    Left NotAHeadPolicy

  -- check that configured CP is present in the datum
  unless (expectedCP == contestationPeriod) $
    Left CPMismatch

  -- check that our party is present in the datum
  unless (party `elem` offChainParties) $
    Left OwnPartyMissing

  -- check that configured parties are matched in the datum
  unless (containsSameElements offChainParties allConfiguredParties) $
    Left PartiesMismatch

  -- pub key hashes of all configured participants == the token names of PTs
  unless (containsSameElements configuredTokenNames (mintedTokenNames headId)) $
    Left PTsNotMintedCorrectly

  pure
    InitObservation
      { threadOutput =
          InitialThreadOutput
            { initialThreadUTxO =
                ( mkTxIn tx ix
                , toCtxUTxOTxOut headOut
                , headData
                )
            , initialParties = onChainParties
            , initialContestationPeriod = toChain contestationPeriod
            }
      , initials
      , commits = []
      , headId = mkHeadId headId
      , seedTxIn
      , -- NOTE: we should look into why parties and cp are duplicated in the InitObservation.
        -- They are included: Once in the InitialThreadOutput in their on-chain form, once in
        -- InitObservation in their off-chain form and they are also included in the datum of
        -- the initialThreadUTxO`.. so three times.
        contestationPeriod
      , parties = offChainParties
      }
 where
  allConfiguredParties = party : otherParties

  configuredTokenNames = assetNameFromVerificationKey <$> cardanoKeys

  containsSameElements a b = Set.fromList a == Set.fromList b

  maybeLeft e = maybe (Left e) Right

  headOutput = \case
    (ix, out@(TxOut addr _ (TxOutDatumInTx d) _)) -> do
      guard $ addr == headAddress
      (ix,out,d,) <$> fromScriptData d
    _ -> Nothing

  headAddress =
    mkScriptAddress @PlutusScriptV2 networkId $
      fromPlutusScript Head.validatorScript

  indexedOutputs = zip [0 ..] (txOuts' tx)

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    mapMaybe
      ( \(i, o) -> do
          dat <- txOutScriptData o
          pure (mkTxIn tx i, toCtxUTxOTxOut o, dat)
      )
      initialOutputs

  isInitial (TxOut addr _ _ _) = addr == initialAddress

  initialAddress = mkScriptAddress @PlutusScriptV2 networkId initialScript

  initialScript = fromPlutusScript Initial.validatorScript

  mintedTokenNames headId =
    [ assetName
    | (AssetId policyId assetName, q) <- txMintAssets tx
    , -- NOTE: It is important to check quantity since we want to ensure
    -- the tokens are unique.
    q == 1
    , policyId == headId
    , assetName /= hydraHeadV1AssetName
    ]
