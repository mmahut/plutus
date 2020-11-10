{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-unused-do-bind #-}
module Spec.Crowdfunding(tests) where

import Control.Monad (void)
import qualified Control.Foldl as L
import Control.Monad.Freer (run)
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Log (LogLevel(..))
import           Data.Text.Prettyprint.Doc (Pretty(..), vsep, layoutPretty, defaultLayoutOptions)
import           Data.Text.Prettyprint.Doc.Render.Text           (renderStrict)
import           Data.ByteString.Lazy                                  (ByteString)
import qualified Data.ByteString.Lazy                                  as BSL
import           Data.Foldable                                         (traverse_)
import qualified Data.Text.Encoding                                    as T
import           Spec.Lib                                              (timesFeeAdjust)
import qualified Spec.Lib                                              as Lib
import           Test.Tasty
import           Test.Tasty.Golden                                     (goldenVsString)
import qualified Test.Tasty.HUnit                                      as HUnit

import           Language.Plutus.Contract hiding (runError)
import qualified Language.Plutus.Contract.Effects.AwaitSlot            as AwaitSlot
import           Language.Plutus.Contract.Test
import qualified Language.Plutus.Contract.Trace                        as Trace
import qualified Language.PlutusTx                                     as PlutusTx
import           Language.PlutusTx.Coordination.Contracts.Crowdfunding
import           Language.PlutusTx.Lattice
import qualified Language.PlutusTx.Prelude                             as PlutusTx
import qualified Ledger.Ada                                            as Ada
import           Ledger.Slot                                           (Slot (..))
import qualified Plutus.Trace.Emulator    as Trace
import Plutus.Trace.Emulator (EmulatorTrace)
import Wallet.Emulator.Stream (takeUntilSlot, filterLogLevel, foldEmulatorStreamM)
import qualified Wallet.Emulator.Folds as Folds
import qualified Streaming.Prelude as S

w1, w2, w3, w4 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4

theContract :: Contract CrowdfundingSchema ContractError ()
theContract = crowdfunding theCampaign

tests :: TestTree
tests = testGroup "crowdfunding"
    [ checkPredicate defaultCheckOptions "Expose 'contribute' and 'scheduleCollection' endpoints"
        (endpointAvailable @"contribute" theContract (Trace.walletInstanceTag w1)
        .&&. endpointAvailable @"schedule collection" theContract (Trace.walletInstanceTag w1)
        )
        $ void (Trace.activateContractWallet w1 theContract)

    , checkPredicate defaultCheckOptions "make contribution"
        -- theContract
        (walletFundsChange w1 (1 `timesFeeAdjust` (-10)))
        $ let contribution = Ada.lovelaceValueOf 10
          in makeContribution w1 contribution

    , checkPredicate defaultCheckOptions "make contributions and collect"
        -- theContract
        (walletFundsChange w1 (1 `timesFeeAdjust` 21))
        $ successfulCampaign

    , checkPredicate defaultCheckOptions "cannot collect money too early"
        -- theContract
        (walletFundsChange w1 PlutusTx.zero
        .&&. assertNoFailedTransactions)
        $ do
            startCampaign
            makeContribution w2 (Ada.lovelaceValueOf 10)
            makeContribution w3 (Ada.lovelaceValueOf 10)
            makeContribution w4 (Ada.lovelaceValueOf 1)
            -- Tell the contract we're at slot 21, causing the transaction to be submitted
            -- FIXME
            -- Trace.addEvent @AwaitSlot.SlotSymbol w1 (AwaitSlot.event 21)
            -- This submits the transaction to the blockchain. Normally, the transaction would
            -- be validated right away and the funds of wallet 1 would increase. In this case
            -- the transaction is not validated because it has a validity interval that begins
            -- _after_ the campaign deadline.
            -- >> Trace.handleBlockchainEvents w1

    , checkPredicate defaultCheckOptions "cannot collect money too late"
        (walletFundsChange w1 PlutusTx.zero
        .&&. assertNoFailedTransactions)
        $ do
            startCampaign
            makeContribution w2 (Ada.lovelaceValueOf 10)
            makeContribution w3 (Ada.lovelaceValueOf 10)
            makeContribution w4 (Ada.lovelaceValueOf 1)
            -- Add some blocks to bring the total up to 31
            -- (that is, above the collection deadline)
            void $ Trace.waitUntilSlot (Slot 31)
            -- FIXME
            -- Then inform the wallet. It's too late to collect the funds
            -- now.
            -- >> Trace.notifySlot w1
            -- >> Trace.handleBlockchainEvents w1
            -- >> Trace.addBlocks 1

    , checkPredicate defaultCheckOptions "cannot collect unless notified"
        (walletFundsChange w1 PlutusTx.zero)
        $ do
            startCampaign
            makeContribution w2 (Ada.lovelaceValueOf 10)
            makeContribution w3 (Ada.lovelaceValueOf 10)
            makeContribution w4 (Ada.lovelaceValueOf 1)
            -- FIXME
            -- Trace.addBlocks' Trace.DontSendSlotNotifications 18
            -- The contributions could be collected now, but without
            -- the slot notifications, wallet 1 is not aware that the
            -- time has come, so it does not submit the transaction.
            -- Trace.handleBlockchainEventsOptions
                    -- Trace.defaultHandleBlockchainEventsOptions{Trace.slotNotifications=Trace.DontSendSlotNotifications}
                    -- w1

    , checkPredicate defaultCheckOptions "can claim a refund"
        (walletFundsChange w2 (2 `timesFeeAdjust` 0)
        .&&. walletFundsChange w3 (2 `timesFeeAdjust` 0))
        $ do
            startCampaign
            makeContribution w2 (Ada.lovelaceValueOf 5)
            makeContribution w3 (Ada.lovelaceValueOf 5)
            -- FIXME
            -- >> Trace.addBlocksUntil' Trace.DontSendSlotNotifications (Slot 30)
            -- >> Trace.notifySlot w2
            -- >> Trace.notifySlot w3
            -- >> traverse_ Trace.handleBlockchainEvents [w2, w3]
            -- >> Trace.addBlocks 1

    , Lib.goldenPir "test/Spec/crowdfunding.pir" $$(PlutusTx.compile [|| mkValidator ||])
    ,   let
            deadline = 10
            target = Ada.lovelaceValueOf 1000
            collectionDeadline = 15
            owner = w1
            cmp = mkCampaign deadline target collectionDeadline owner
        in HUnit.testCase "script size is reasonable" (Lib.reasonable (contributionScript cmp) 30000)

    , goldenVsString
        "renders the context of a trace sensibly"
        "test/Spec/crowdfundingTestOutput.txt"
        (renderPredicate successfulCampaign)

    , let con :: Contract BlockchainActions ContractError () = throwError "something went wrong" in
        goldenVsString
        "renders an error sensibly"
        "test/Spec/contractError.txt"
        (renderPredicate (void $ Trace.activateContractWallet w1 con))
    ]

renderPredicate :: EmulatorTrace () -> IO ByteString
renderPredicate trace = do
    let result =
            run
            $ foldEmulatorStreamM (L.generalize $ Folds.instanceLog (Trace.walletInstanceTag w1))
            $ filterLogLevel Info
            $ takeUntilSlot 20
            $ Trace.runEmulatorStream Trace.defaultEmulatorConfig trace
    pure $ BSL.fromStrict $ T.encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions $ vsep $ fmap pretty $ S.fst' result
