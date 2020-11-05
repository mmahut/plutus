{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-strictness  #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
module Spec.MultiSigStateMachine(tests) where

import           Data.Foldable                                                 (traverse_)
import           Test.Tasty                                                    (TestTree, testGroup)
import qualified Test.Tasty.HUnit                                              as HUnit

import           Spec.Lib                                                      as Lib

import           Language.PlutusTx.Lattice
import qualified Ledger
import qualified Ledger.Ada                                                    as Ada
import qualified Ledger.Typed.Scripts                                          as Scripts
import qualified Wallet.Emulator                                               as EM

import           Language.Plutus.Contract.Test
import qualified Language.PlutusTx                                             as PlutusTx
import qualified Plutus.Trace.Emulator    as Trace
import Plutus.Trace.Emulator (EmulatorTrace)
import           Language.PlutusTx.Coordination.Contracts.MultiSigStateMachine (MultiSigError, MultiSigSchema)
import qualified Language.PlutusTx.Coordination.Contracts.MultiSigStateMachine as MS
import qualified Plutus.Trace.Emulator    as Trace
import Plutus.Trace.Emulator (EmulatorTrace)

tests :: TestTree
tests =
    testGroup "multi sig state machine tests"
    [ checkPredicate defaultCheckOptions "lock, propose, sign 3x, pay - SUCCESS"
        (assertNoFailedTransactions
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
        .&&. walletFundsChange w2 (Ada.lovelaceValueOf 5))
        (lockProposeSignPay 3 1)

    , checkPredicate defaultCheckOptions "lock, propose, sign 2x, pay - FAILURE"
        (assertContractError (MS.contract params) (Trace.walletInstanceTag w1) (\case { MS.MSStateMachineError{} -> True; _ -> False}) "contract should fail"
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
        .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0))
        (lockProposeSignPay 2 1)

    , checkPredicate defaultCheckOptions "lock, propose, sign 3x, pay x2 - SUCCESS"
        (assertNoFailedTransactions
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
        .&&. walletFundsChange w2 (Ada.lovelaceValueOf 10))
        (lockProposeSignPay 3 2)

    , checkPredicate defaultCheckOptions "lock, propose, sign 3x, pay x3 - FAILURE"
        (assertContractError (MS.contract params) (Trace.walletInstanceTag w2) (\case { MS.MSStateMachineError{} -> True; _ -> False}) "contract should fail"
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
        .&&. walletFundsChange w2 (Ada.lovelaceValueOf 10))
        -- (lockProposeSignPay 3 2 >> callEndpoint @"propose-payment" w2 payment >> handleBlockchainEvents w2)
        (lockProposeSignPay 3 2) -- FIXME

    , Lib.goldenPir "test/Spec/multisigStateMachine.pir" $$(PlutusTx.compile [|| MS.mkValidator ||])
    , HUnit.testCase "script size is reasonable" (Lib.reasonable (Scripts.validatorScript $ MS.scriptInstance params) 51000)
    ]

w1, w2, w3 :: EM.Wallet
w1 = EM.Wallet 1
w2 = EM.Wallet 2
w3 = EM.Wallet 3

-- | A multisig contract that requires 3 out of 5 signatures
params :: MS.Params
params = MS.Params keys 3 where
    keys = Ledger.pubKeyHash . EM.walletPubKey . EM.Wallet <$> [1..5]

-- | A payment of 5 Ada to the public key address of wallet 2
payment :: MS.Payment
payment =
    MS.Payment
        { MS.paymentAmount    = Ada.lovelaceValueOf 5
        , MS.paymentRecipient = Ledger.pubKeyHash $ EM.walletPubKey w2
        , MS.paymentDeadline  = 20
        }

-- | Lock some funds in the contract, then propose the payment
--   'payment', then call @"add-signature"@ a number of times and
--   finally call @"pay"@ a number of times.
lockProposeSignPay :: Integer -> Integer -> EmulatorTrace ()
lockProposeSignPay signatures rounds = do
    let wallets = EM.Wallet <$> [1..signatures]
        activate w = Trace.activateContractWallet w (MS.contract @MS.MultiSigError params)

    -- the 'proposeSignPay' trace needs at least 2 signatures
    handle1 <- activate (EM.Wallet 1)
    handle2 <- activate (EM.Wallet 2)
    handles <- traverse activate (drop 2 wallets)
    _ <- Trace.callEndpoint @"lock" w1 handle1 (Ada.lovelaceValueOf 10)
    _ <- Trace.waitNSlots 1
    let proposeSignPay = do
            Trace.callEndpoint @"propose-payment" w2 handle2 payment
            Trace.waitNSlots 1
            -- Call @"add-signature"@ @signatures@ times
            traverse_ (\(wllt, hdl) -> Trace.callEndpoint @"add-signature" wllt hdl () >> Trace.waitNSlots 1) (zip wallets (handle1:handle2:handles))

            -- Call @"pay"@ on wallet 1
            Trace.callEndpoint @"pay" w1 handle1 ()
            Trace.waitNSlots 1

    traverse_ (\_ -> proposeSignPay) [1..rounds]
