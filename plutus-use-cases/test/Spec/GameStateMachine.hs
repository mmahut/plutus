{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Spec.GameStateMachine(tests) where

import           Test.Tasty
import qualified Test.Tasty.HUnit                                          as HUnit

import qualified Spec.Lib                                                  as Lib

import qualified Language.PlutusTx                                         as PlutusTx

import Control.Monad (void)
import           Language.Plutus.Contract.Test
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine as G
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                                as Ada
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value                                              (Value)
import qualified Wallet.Emulator                                           as EM
import qualified Plutus.Trace.Emulator    as Trace
import Plutus.Trace.Emulator (EmulatorTrace)

tests :: TestTree
tests =
    testGroup "state machine tests"
    [ checkPredicate defaultCheckOptions "run a successful game trace"
        (walletFundsChange w2 (Ada.lovelaceValueOf 3 <> gameTokenVal)
        .&&. valueAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 5 ==)
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-8)))
        successTrace

    , checkPredicate defaultCheckOptions "run a 2nd successful game trace"
        (walletFundsChange w2 (Ada.lovelaceValueOf 3)
        .&&. valueAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 1 ==)
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-4) <> gameTokenVal))
        $ do
            successTrace
            Trace.payToWallet w2 w1 gameTokenVal
            Trace.waitNSlots 1
            hdl3 <- Trace.activateContractWallet w3 G.contract
            Trace.callEndpoint @"guess" w1 hdl3 GuessArgs{guessArgsOldSecret="new secret", guessArgsNewSecret="hello", guessArgsValueTakenOut=Ada.lovelaceValueOf 4}
            void $ Trace.waitNSlots 1

    , checkPredicate defaultCheckOptions "run a failed trace"
        (walletFundsChange w2 gameTokenVal
        .&&. valueAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 8 ==)
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-8)))
        $ do
            hdl <- Trace.activateContractWallet w1 G.contract
            Trace.callEndpoint @"lock" w1 hdl LockArgs{lockArgsSecret="hello", lockArgsValue= Ada.lovelaceValueOf 8}
            _ <- Trace.waitNSlots 2
            Trace.payToWallet w1 w2 gameTokenVal
            _ <- Trace.waitNSlots 1
            hdl2 <- Trace.activateContractWallet w2 G.contract
            _ <- Trace.callEndpoint @"guess" w2 hdl2 GuessArgs{guessArgsOldSecret="hola", guessArgsNewSecret="new secret", guessArgsValueTakenOut=Ada.lovelaceValueOf 3}
            void $ Trace.waitNSlots 1

    , Lib.goldenPir "test/Spec/gameStateMachine.pir" $$(PlutusTx.compile [|| mkValidator ||])

    , HUnit.testCase "script size is reasonable"
        (Lib.reasonable (Scripts.validatorScript G.scriptInstance) 49000)

    ]

initialVal :: Value
initialVal = Ada.adaValueOf 10

w1 :: EM.Wallet
w1 = EM.Wallet 1

w2 :: EM.Wallet
w2 = EM.Wallet 2

w3 :: EM.Wallet
w3 = EM.Wallet 3

successTrace :: EmulatorTrace ()
successTrace = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" w1 hdl LockArgs{lockArgsSecret="hello", lockArgsValue= Ada.lovelaceValueOf 8}
    _ <- Trace.waitNSlots 2
    Trace.payToWallet w1 w2 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 G.contract
    Trace.callEndpoint @"guess" w2 hdl2 GuessArgs{guessArgsOldSecret="hello", guessArgsNewSecret="new secret", guessArgsValueTakenOut=Ada.lovelaceValueOf 3}
    void $ Trace.waitNSlots 1

gameTokenVal :: Value
gameTokenVal =
    let sym = Scripts.monetaryPolicyHash G.scriptInstance
    in G.token sym "guess"
