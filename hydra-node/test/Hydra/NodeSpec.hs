{-# LANGUAGE DerivingStrategies #-}
module Hydra.NodeSpec where

import Cardano.Prelude
import Hydra.Node
import Test.Hspec (Spec, around, describe, it, shouldReturn)

spec :: Spec
spec = around startStopNode $
  describe "Live Hydra Node" $ do
    it "returns Ready when asked status" $ \node -> do
      nodeStatus node `shouldReturn` Ready

startStopNode :: (HydraNode -> IO ()) -> IO ()
startStopNode = bracket startHydraNode stopHydraNode
