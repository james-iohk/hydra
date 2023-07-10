{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx (
  module Helpers,
  module Init,
  module Commit,
  module CollectCom,
  module Abort,
  module Close,
  module Fanout,
)
where

import Plutus.Orphans ()

import Hydra.Chain.Direct.Tx.Abort as Abort
import Hydra.Chain.Direct.Tx.Close as Close
import Hydra.Chain.Direct.Tx.CollectCom as CollectCom
import Hydra.Chain.Direct.Tx.Commit as Commit
import Hydra.Chain.Direct.Tx.Fanout as Fanout
import Hydra.Chain.Direct.Tx.Helpers as Helpers
import Hydra.Chain.Direct.Tx.Init as Init


