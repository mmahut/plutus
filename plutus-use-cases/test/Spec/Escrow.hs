{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Spec.Escrow where

import           Control.Monad                                   (void)

import           Language.Plutus.Contract
import           Language.Plutus.Contract.Test
import           Ledger                                          (pubKeyHash)
import qualified Ledger.Ada                                      as Ada
import qualified Ledger.Typed.Scripts                            as Scripts
import qualified Spec.Lib                                        as Lib

import           Language.PlutusTx.Coordination.Contracts.Escrow
import           Language.PlutusTx.Lattice
import qualified Plutus.Trace.Emulator    as Trace

import           Test.Tasty
import qualified Test.Tasty.HUnit                                as HUnit

tests :: TestTree
tests = testGroup "escrow"
    [ let con = void $ payEp @EscrowSchema @EscrowError escrowParams in
      checkPredicate defaultCheckOptions "can pay"
        ( assertDone con (Trace.walletInstanceTag w1) (const True) "escrow pay not done"
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
        )
        $ do
          hdl <- Trace.activateContractWallet w1 con
          Trace.callEndpoint @"pay-escrow" w1 hdl (Ada.lovelaceValueOf 10)
          void $ Trace.waitNSlots 1

    , let con = void $ selectEither (payEp  @EscrowSchema @EscrowError escrowParams) (redeemEp escrowParams) in
      checkPredicate defaultCheckOptions "can redeem"
        ( assertDone con (Trace.walletInstanceTag w3) (const True) "escrow redeem not done"
          .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
          .&&. walletFundsChange w2  (Ada.lovelaceValueOf 10)
          .&&. walletFundsChange w3 mempty
        )
        $ do
          hdl1 <- Trace.activateContractWallet w1 con
          hdl2 <- Trace.activateContractWallet w2 con
          hdl3 <- Trace.activateContractWallet w3 con

          Trace.callEndpoint @"pay-escrow" w1 hdl1 (Ada.lovelaceValueOf 20)
          Trace.callEndpoint @"pay-escrow" w2 hdl2 (Ada.lovelaceValueOf 10)
          Trace.waitNSlots 1
          Trace.callEndpoint @"redeem-escrow" w3 hdl3 ()
          void $ Trace.waitNSlots 1

    , let con = (void $ both (payEp @EscrowSchema @EscrowError escrowParams) (redeemEp escrowParams)) in
      checkPredicate defaultCheckOptions "can redeem even if more money than required has been paid in"

          -- in this test case we pay in a total of 40 lovelace (10 more than required), for
          -- the same contract as before, requiring 10 lovelace to go to wallet 1 and 20 to
          -- wallet 2.
          --
          -- The scenario is
          -- * Wallet 1 contributes 20
          -- * Wallet 2 contributes 10
          -- * Wallet 3 contributes 10
          -- * Wallet 1 is going to redeem the payments
          --

          -- Wallet 1 pays 20 and receives 10 from the escrow contract and another 10
          -- in excess inputs
          ( walletFundsChange w1 (Ada.lovelaceValueOf 0)

          -- Wallet 2 pays 10 and receives 20, as per the contract.
            .&&. walletFundsChange w2 (Ada.lovelaceValueOf 10)

          -- Wallet 3 pays 10 and doesn't receive anything.
            .&&. walletFundsChange w3 (Ada.lovelaceValueOf (-10))
          )

          $ do
            hdl1 <- Trace.activateContractWallet w1 con
            hdl2 <- Trace.activateContractWallet w2 con
            hdl3 <- Trace.activateContractWallet w3 con
            Trace.callEndpoint @"pay-escrow" w1 hdl1 (Ada.lovelaceValueOf 20)
            Trace.callEndpoint @"pay-escrow" w2 hdl2 (Ada.lovelaceValueOf 10)
            Trace.callEndpoint @"pay-escrow" w3 hdl3 (Ada.lovelaceValueOf 10)
            Trace.waitNSlots 1
            Trace.callEndpoint @"redeem-escrow" w1 hdl1 ()
            void $ Trace.waitNSlots 1

    , let con = void $ payEp  @EscrowSchema @EscrowError escrowParams >> refundEp escrowParams in
      checkPredicate defaultCheckOptions "can refund"
        ( walletFundsChange w1 mempty
          .&&. assertDone con (Trace.walletInstanceTag w1) (const True) "refund should succeed")
        $ do
            hdl1 <- Trace.activateContractWallet w1 con
            Trace.callEndpoint @"pay-escrow" w1 hdl1 (Ada.lovelaceValueOf 20) 
            Trace.waitNSlots 200
            Trace.callEndpoint @"refund-escrow" w1 hdl1 ()
            void $ Trace.waitNSlots 1

    , HUnit.testCase "script size is reasonable" (Lib.reasonable (Scripts.validatorScript $ scriptInstance escrowParams) 32000)
    ]

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

escrowParams :: EscrowParams d
escrowParams =
  EscrowParams
    { escrowDeadline = 200
    , escrowTargets  =
        [ payToPubKeyTarget (pubKeyHash $ walletPubKey w1) (Ada.lovelaceValueOf 10)
        , payToPubKeyTarget (pubKeyHash $ walletPubKey w2) (Ada.lovelaceValueOf 20)
        ]
    }
