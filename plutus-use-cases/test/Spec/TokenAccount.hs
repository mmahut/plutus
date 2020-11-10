{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Spec.TokenAccount(tests, assertAccountBalance) where

import           Test.Tasty

import Control.Monad (void)
import Control.Monad.Freer (run)
import Control.Monad.Freer.Error (runError)
import Language.Plutus.Contract (Contract)
import Wallet.Emulator.Stream (foldEmulatorStreamM, takeUntilSlot)
import qualified Streaming.Prelude as S
import           Language.Plutus.Contract.Test
import           Language.PlutusTx.Lattice
import qualified Ledger
import qualified Ledger.Ada                                            as Ada
import           Ledger.Value                                          (TokenName, Value)

import           Language.PlutusTx.Coordination.Contracts.TokenAccount (Account (..), TokenAccountError,
                                                                        TokenAccountSchema, tokenAccountContract)
import qualified Language.PlutusTx.Coordination.Contracts.TokenAccount as Accounts
import qualified Plutus.Trace.Emulator    as Trace
import qualified Wallet.Emulator.Folds as Folds

tests :: TestTree
tests = testGroup "token account"
    [ checkPredicate defaultCheckOptions "Create a token account"
        (assertNoFailedTransactions
        .&&. assertNotDone contract (Trace.walletInstanceTag w1) "contract should not have any errors"
        .&&. walletFundsChange w1 theToken)
        $ do
            hdl <- Trace.activateContractWallet w1 contract
            Trace.callEndpoint @"new-account" w1 hdl (tokenName, Ledger.pubKeyHash $ walletPubKey w1)
            void $ Trace.waitNSlots 2

    , checkPredicate defaultCheckOptions "Pay into the account"
        (assertNoFailedTransactions
        .&&. assertNotDone contract (Trace.walletInstanceTag w1) "contract should not have any errors"
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10) <> theToken))
        $ do
            hdl <- Trace.activateContractWallet w1 contract
            Trace.callEndpoint @"new-account" w1 hdl (tokenName, Ledger.pubKeyHash $ walletPubKey w1)
            Trace.waitNSlots 2
            Trace.callEndpoint @"pay" w1 hdl (account, Ada.lovelaceValueOf 10)
            void $ Trace.waitNSlots 1

    , checkPredicate defaultCheckOptions "Transfer & redeem all funds"
        (assertNoFailedTransactions
        .&&. assertNotDone contract (Trace.walletInstanceTag w1) "contract should not have any errors"
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
        .&&. walletFundsChange w2 (theToken <> Ada.lovelaceValueOf 10))
        $ do
            hdl <- Trace.activateContractWallet w1 contract
            hdl2 <- Trace.activateContractWallet w2 contract
            Trace.callEndpoint @"new-account" w1 hdl (tokenName, Ledger.pubKeyHash $ walletPubKey w1)
            Trace.waitNSlots 2
            Trace.callEndpoint @"pay" w1 hdl (account, Ada.lovelaceValueOf 10)
            Trace.waitNSlots 1
            Trace.payToWallet w1 w2 theToken
            Trace.waitNSlots 1
            Trace.callEndpoint @"redeem" w2 hdl2 (account, Ledger.pubKeyHash $ walletPubKey w2)
            void $ Trace.waitNSlots 1

    ]

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

tokenName :: TokenName
tokenName = "test token"

contract :: Contract TokenAccountSchema TokenAccountError ()
contract = tokenAccountContract

account :: Account
account =
    let con = Accounts.newAccount @TokenAccountSchema @TokenAccountError tokenName (Ledger.pubKeyHash $ walletPubKey w1)
        fld = Folds.instanceOutcome con (Trace.walletInstanceTag w1)
        trace = Trace.activateContractWallet w1 (void con) >> Trace.waitNSlots 2
        getOutcome (Done a) = a
        getOutcome _        = error "not finished"
    in
    either (error . show) (getOutcome . S.fst')
        $ run
        $ runError @Folds.EmulatorFoldErr
        $ foldEmulatorStreamM fld
        $ takeUntilSlot 10
        $ Trace.runEmulatorStream Trace.defaultEmulatorConfig trace

theToken :: Value
theToken = Accounts.accountToken account

-- | Check that the balance of the given account satisfies a predicate.
assertAccountBalance :: Account -> (Value -> Bool) -> TracePredicate
assertAccountBalance acc = valueAtAddress (Accounts.address acc)
