{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Tx.Helpers where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import Hydra.Chain (HeadId (..))
import Hydra.Contract.Util (hydraHeadV1)
import qualified Hydra.Data.ContestationPeriod as OnChain
import qualified Hydra.Data.Party as OnChain
import Plutus.Orphans ()
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), fromBuiltin, toBuiltin)
import qualified PlutusLedgerApi.V2 as Plutus

-- | Needed on-chain data to create Head transactions.
type UTxOWithScript = (TxIn, TxOut CtxUTxO, HashableScriptData)

newtype UTxOHash = UTxOHash ByteString
  deriving (Eq, Show, Generic)

instance ToJSON UTxOHash where
  toJSON (UTxOHash bytes) =
    Aeson.String . decodeUtf8 $ Base16.encode bytes

instance FromJSON UTxOHash where
  parseJSON = Aeson.withText "UTxOHash" $ \cborText ->
    case Base16.decode $ encodeUtf8 cborText of
      Left e -> fail e
      Right bs -> pure $ UTxOHash bs

-- | Representation of the Head output after an Init transaction.
data InitialThreadOutput = InitialThreadOutput
  { initialThreadUTxO :: UTxOWithScript
  , initialContestationPeriod :: OnChain.ContestationPeriod
  , initialParties :: [OnChain.Party]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Representation of the Head output after a CollectCom transaction.
data OpenThreadOutput = OpenThreadOutput
  { openThreadUTxO :: UTxOWithScript
  , openContestationPeriod :: OnChain.ContestationPeriod
  , openParties :: [OnChain.Party]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ClosedThreadOutput = ClosedThreadOutput
  { closedThreadUTxO :: UTxOWithScript
  , closedParties :: [OnChain.Party]
  , closedContestationDeadline :: Plutus.POSIXTime
  , closedContesters :: [Plutus.PubKeyHash]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

hydraHeadV1AssetName :: AssetName
hydraHeadV1AssetName = AssetName (fromBuiltin hydraHeadV1)

-- FIXME: sould not be hardcoded
headValue :: Value
headValue = lovelaceToValue (Lovelace 2_000_000)


-- * Helpers

mkHeadId :: PolicyId -> HeadId
mkHeadId =
  HeadId . serialiseToRawBytes

headIdToCurrencySymbol :: HeadId -> CurrencySymbol
headIdToCurrencySymbol (HeadId headId) = CurrencySymbol (toBuiltin headId)

headTokensFromValue :: PlutusScript -> Value -> [(AssetName, Quantity)]
headTokensFromValue headTokenScript v =
  [ (assetName, q)
  | (AssetId pid assetName, q) <- valueToList v
  , pid == scriptPolicyId (PlutusScript headTokenScript)
  ]

assetNameFromVerificationKey :: VerificationKey PaymentKey -> AssetName
assetNameFromVerificationKey =
  AssetName . serialiseToRawBytes . verificationKeyHash

-- | Find first occurrence including a transformation.
findFirst :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findFirst fn = getFirst . foldMap (First . fn)

findHeadAssetId :: TxOut ctx -> Maybe (PolicyId, AssetName)
findHeadAssetId txOut =
  flip findFirst (valueToList $ txOutValue txOut) $ \case
    (AssetId pid aname, q)
      | aname == hydraHeadV1AssetName && q == 1 ->
          Just (pid, aname)
    _ ->
      Nothing

-- | Find (if it exists) the head identifier contained in given `TxOut`.
findStateToken :: TxOut ctx -> Maybe HeadId
findStateToken =
  fmap (mkHeadId . fst) . findHeadAssetId


