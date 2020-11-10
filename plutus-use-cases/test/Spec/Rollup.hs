{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Spec.Rollup where

import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer (run)
import qualified Control.Foldl as L
import           Data.Aeson                                            (FromJSON)
import           Data.ByteString.Lazy                                  (ByteString)
import qualified Data.ByteString.Lazy                                  as LBS
import qualified Data.Map                                              as Map
import qualified Data.Row.Internal                                     as V
import           Data.Text.Encoding                                    (encodeUtf8)

import           Language.Plutus.Contract hiding (runError)
import           Language.Plutus.Contract.Schema                       (Input, Output)
import           Language.Plutus.Contract.Trace
import           Ledger                                                (pubKeyHash)

import           Language.PlutusTx.Coordination.Contracts.Crowdfunding
import           Language.PlutusTx.Coordination.Contracts.Game
import           Language.PlutusTx.Coordination.Contracts.Vesting
import qualified Spec.Vesting

import           Test.Tasty                                            (TestTree, testGroup)
import           Test.Tasty.Golden                                     (goldenVsString)
import           Test.Tasty.HUnit                                      (assertFailure)
import qualified Wallet.Emulator.Chain                                 as EM
import           Wallet.Emulator.Types
import qualified Wallet.Emulator.Wallet                                as EM
import           Wallet.Rollup.Render                                  (showBlockchainFold)
import Plutus.Trace.Emulator (runEmulatorStream, EmulatorTrace)
import Wallet.Emulator.Stream (foldEmulatorStreamM, takeUntilSlot)
import qualified Plutus.Trace.Emulator    as Trace
import qualified Streaming.Prelude as S

tests :: TestTree
tests = testGroup "showBlockchain"
     [ goldenVsString
          "renders a crowdfunding scenario sensibly"
          "test/Spec/renderCrowdfunding.txt"
          (render successfulCampaign)
     , goldenVsString
          "renders a guess scenario sensibly"
          "test/Spec/renderGuess.txt"
          (render guessTrace)
     , goldenVsString
          "renders a vesting scenario sensibly"
          "test/Spec/renderVesting.txt"
          (render Spec.Vesting.retrieveFundsTrace)
     ]

render :: forall a. EmulatorTrace a -> IO ByteString
render trace = do
    let result = 
               S.fst'
               $ run
               $ foldEmulatorStreamM (L.generalize (showBlockchainFold allWallets))
               $ takeUntilSlot 20
               $ runEmulatorStream Trace.defaultEmulatorConfig trace
        allWallets = fmap (\w -> (pubKeyHash (walletPubKey w), w)) [Wallet 1, Wallet 2]
    case result of
        Left err -> assertFailure $ show err
        Right rendered -> pure $ LBS.fromStrict $ encodeUtf8 rendered
