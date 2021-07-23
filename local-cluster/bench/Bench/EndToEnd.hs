{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Bench.EndToEnd where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import Control.Lens ((^?))
import Control.Monad.Class.MonadSTM (
  MonadSTM (readTVarIO),
  modifyTVar,
  newTVarIO,
 )
import Data.Aeson (Value, (.=), encodeFile)
import Data.Aeson.Lens (key, _Array, _Number)
import qualified Data.Map as Map
import Data.Scientific (floatingOrInteger)
import Data.Set ((\\))
import qualified Data.Set as Set
import Hydra.Ledger (Tx, TxId, txId)
import Hydra.Ledger.Simple (SimpleTx, genSequenceOfValidTransactions, utxoRefs)
import Hydra.Logging (showLogsOnFailure)
import HydraNode (
  HydraClient,
  failAfter,
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraNode,
  withMockChain,
 )
import Test.QuickCheck (generate)
import Test.QuickCheck.Gen (scale)

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

data Event = Event
  { submittedAt :: UTCTime
  , confirmedAt :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON)

bench :: IO ()
bench = do
  tmpDir <- createSystemTempDirectory "bench"
  registry <- newTVarIO mempty :: IO (TVar IO (Map.Map (TxId SimpleTx) Event))

  -- NOTE(SN): Maybe put these into a golden data set as soon as we are happy
  let initialUtxo = utxoRefs [1, 2, 3]
  txs <- generate $ scale (* 100) $ genSequenceOfValidTransactions initialUtxo
  let txsFile = tmpDir </> "txs.json"
  putStrLn $ "Writing transactions to: " <> txsFile
  encodeFile txsFile txs

  failAfter 300 $
    showLogsOnFailure $ \tracer ->
      withMockChain $ \chainPorts ->
        withHydraNode tracer tmpDir chainPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
          withHydraNode tracer tmpDir chainPorts 2 bobSk [aliceVk, carolVk] $ \n2 ->
            withHydraNode tracer tmpDir chainPorts 3 carolSk [aliceVk, bobVk] $ \n3 -> do
              waitForNodesConnected tracer [n1, n2, n3]
              let contestationPeriod = 10 -- TODO: Should be part of init
              send n1 $ input "init" []
              waitFor tracer 3 [n1, n2, n3] $
                output "readyToCommit" ["parties" .= [int 10, 20, 30]]
              send n1 $ input "commit" ["utxo" .= [int 1]]
              send n2 $ input "commit" ["utxo" .= [int 2]]
              send n3 $ input "commit" ["utxo" .= [int 3]]

              waitFor tracer 3 [n1, n2, n3] $ output "headIsOpen" ["utxo" .= [int 1, 2, 3]]

              for_ txs (\tx -> newTx registry n1 tx >> threadDelay 0.001)
                `concurrently_` waitForAllConfirmations n1 registry txs

              send n1 $ input "close" []
              waitMatch (contestationPeriod + 3) n1 $ \v ->
                guard (v ^? key "output" == Just "headIsFinalized")

  res <- mapMaybe analyze . Map.toList <$> readTVarIO registry
  let resFile = tmpDir </> "results.json"
  putStrLn $ "Writing results to: " <> resFile
  encodeFile resFile res

--
-- Helpers
--

int :: Int -> Int
int = id

type TransactionId = Integer
type TransactionInput = Int
type TransactionOutput = Int

newTx ::
  Tx tx =>
  TVar IO (Map.Map (TxId tx) Event) ->
  HydraClient ->
  tx ->
  IO ()
newTx registry client tx = do
  now <- getCurrentTime
  atomically $
    modifyTVar registry $
      Map.insert (txId tx) $
        Event
          { submittedAt = now
          , confirmedAt = Nothing
          }
  send client $ input "newTransaction" ["transaction" .= tx]

waitForAllConfirmations :: HydraClient -> TVar IO (Map.Map (TxId SimpleTx) Event) -> [SimpleTx] -> IO ()
waitForAllConfirmations n1 registry txs =
  go allIds
 where
  allIds = Set.fromList $ map txId txs

  go remainingIds
    | Set.null remainingIds = pure ()
    | otherwise = do
      res <- waitMatch 10 n1 $ \v -> do
        guard (v ^? key "output" == Just "snapshotConfirmed")
        v ^? key "snapshot" . key "confirmedTransactions" . _Array
      confirmedIds <- mapM (confirmTx registry) res
      go (remainingIds \\ Set.fromList (toList confirmedIds))

confirmTx ::
  TVar IO (Map.Map (TxId SimpleTx) Event) ->
  Value ->
  IO (TxId SimpleTx)
confirmTx registry tx = do
  case floatingOrInteger @Double <$> tx ^? key "id" . _Number of
    Just (Right identifier) -> do
      now <- getCurrentTime
      atomically $
        modifyTVar registry $
          Map.adjust (\e -> e{confirmedAt = Just now}) identifier
      pure identifier
    _ -> error $ "incorrect Txid" <> show tx

analyze :: (TxId SimpleTx, Event) -> Maybe (UTCTime, NominalDiffTime)
analyze = \case
  (_, Event{submittedAt, confirmedAt = Just conf}) -> Just (submittedAt, conf `diffUTCTime` submittedAt)
  _ -> Nothing
