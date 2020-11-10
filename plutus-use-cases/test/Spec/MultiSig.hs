{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Spec.MultiSig(tests) where

import Control.Monad (void)
import           Language.Plutus.Contract                          (ContractError, Contract)
import           Language.Plutus.Contract.Test
import qualified Language.PlutusTx                                 as PlutusTx
import           Language.PlutusTx.Coordination.Contracts.MultiSig as MS
import qualified Ledger
import qualified Ledger.Ada                                        as Ada
import           Ledger.Index                                      (ValidationError (ScriptFailure))
import           Ledger.Scripts                                    (ScriptError (EvaluationError))
import           Wallet.Emulator.Wallet                    (signWallets)
import qualified Plutus.Trace.Emulator    as Trace
import Plutus.Trace.Emulator (EmulatorTrace)
import           Prelude                                           hiding (not)
import qualified Spec.Lib                                          as Lib
import           Test.Tasty

tests :: TestTree
tests = testGroup "multisig"
    [ checkPredicate defaultCheckOptions "2 out of 5"
        (assertFailedTransaction (\_ err -> case err of {ScriptFailure (EvaluationError ["not enough signatures"]) -> True; _ -> False  }))
        $ do
            hdl <- Trace.activateContractWallet w1 theContract
            Trace.callEndpoint @"lock" w1 hdl (multiSig, Ada.lovelaceValueOf 10)
            Trace.waitNSlots 1
            Trace.setSigningProcess w1 (signWallets [w1, w2])
            Trace.callEndpoint @"unlock" w1 hdl (multiSig, fmap (Ledger.pubKeyHash . walletPubKey) [w1, w2])
            void $ Trace.waitNSlots 1

    , checkPredicate defaultCheckOptions "3 out of 5"
        assertNoFailedTransactions
        $ do
            hdl <- Trace.activateContractWallet w1 theContract
            Trace.callEndpoint @"lock" w1 hdl (multiSig, Ada.lovelaceValueOf 10)
            Trace.waitNSlots 1
            Trace.setSigningProcess w1 (signWallets [w1, w2, w3])
            Trace.callEndpoint @"unlock" w1 hdl (multiSig, fmap (Ledger.pubKeyHash . walletPubKey) [w1, w2, w3])
            void $ Trace.waitNSlots 1

    , Lib.goldenPir "test/Spec/multisig.pir" $$(PlutusTx.compile [|| MS.validate ||])
    ]

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

theContract :: Contract MultiSigSchema ContractError ()
theContract = MS.contract

-- a 'MultiSig' contract that requires three out of five signatures
multiSig :: MultiSig
multiSig = MultiSig
        { signatories = Ledger.pubKeyHash . walletPubKey . Wallet <$> [1..5]
        , minNumSignatures = 3
        }
