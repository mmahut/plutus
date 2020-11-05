{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Spec.Contract(tests) where

import Control.Lens
import           Control.Monad                                 (void, forever)
import Control.Monad.Freer (Eff)
import           Control.Monad.Error.Lens
import           Control.Monad.Freer.Log (LogLevel(..))
import           Control.Monad.Except                          (catchError, throwError)
import           Test.Tasty

import           Language.Plutus.Contract                      as Con
import           Language.Plutus.Contract.Test
import           Language.Plutus.Contract.Util                 (loopM)
import qualified Language.PlutusTx                             as PlutusTx
import           Language.PlutusTx.Lattice
import           Ledger                                        (Address, Slot, PubKey)
import qualified Ledger                                        as Ledger
import qualified Ledger.Ada                                    as Ada
import qualified Ledger.Constraints                            as Constraints
import qualified Ledger.Crypto                                 as Crypto
import           Prelude                                       hiding (not)
import Plutus.Trace.Emulator (callEndpoint, activateContract, ContractInstanceTag, Emulator)
import Plutus.Trace (Trace)
import qualified Plutus.Trace as Trace
import qualified Wallet.Emulator                               as EM

import qualified Language.Plutus.Contract.Effects.AwaitSlot    as AwaitSlot
import           Language.Plutus.Contract.Trace.RequestHandler (maybeToHandler)



tests :: TestTree
tests =
    let run :: Slot -> String -> TracePredicate -> Eff '[Trace Emulator] () -> _
        run sl = checkPredicate (defaultCheckOptions & maxSlot .~ sl & minLogLevel .~ Debug)

        check :: Slot -> String -> Contract Schema ContractError () -> _ -> _
        check sl nm contract pred = run sl nm (pred contract) (void $ activateContract w1 contract tag)
                
        tag :: ContractInstanceTag
        tag = "instance 1"

    in
    testGroup "contracts"
        [ check 1 "awaitSlot" (void $ awaitSlot 10) $ \con ->
            (waitingForSlot con tag 10)

        , check 1 "selectEither" (void $ selectEither (awaitSlot 10) (awaitSlot 5)) $ \con ->
            (waitingForSlot con tag 5)

        , check 1 "until" (void $ void $ awaitSlot 10 `Con.until` 5) $ \con ->
            (waitingForSlot con tag 5)

        , check 1 "both" (void $ Con.both (awaitSlot 10) (awaitSlot 20)) $ \con ->
            (waitingForSlot con tag 10)

        , check 1 "both (2)" (void $ Con.both (awaitSlot 10) (awaitSlot 20)) $ \con ->
            (waitingForSlot con tag 20)

        , check 1 "watchAddressUntil" (void $ watchAddressUntil someAddress 5) $ \con ->
            (waitingForSlot con tag 5)

        , check 1 "endpoint" (endpoint @"ep") $ \con ->
            (endpointAvailable @"ep" con tag)

        , check 1 "forever" (forever $ endpoint @"ep") $ \con ->
            (endpointAvailable @"ep" con tag)

        , let
            oneTwo :: Contract Schema ContractError Int = endpoint @"1" >> endpoint @"2" >> endpoint @"4"
            oneThree :: Contract Schema ContractError Int = endpoint @"1" >> endpoint @"3" >> endpoint @"4"
            con = void (oneTwo `select` oneThree)
          in
            run 1 "alternative"
                (endpointAvailable @"3" con tag
                    .&&. not (endpointAvailable @"2" con tag))
                $ do
                    hdl <- activateContract w1 con tag
                    callEndpoint @"1" w1 hdl 1

        , let theContract :: Contract Schema ContractError () = void $ endpoint @"1" @Int >> endpoint @"2" @Int
          in run 1 "call endpoint (1)"
                (endpointAvailable @"1" theContract tag)
                (void $ activateContract w1 theContract tag)

        , let theContract :: Contract Schema ContractError () = void $ endpoint @"1" @Int >> endpoint @"2" @Int
          in run 1 "call endpoint (2)"
                (endpointAvailable @"2" theContract tag
                    .&&. not (endpointAvailable @"1" theContract tag))
                (activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" w1 hdl 1)
        
        , let theContract :: Contract Schema ContractError () = void $ endpoint @"1" @Int >> endpoint @"2" @Int
          in run 1 "call endpoint (3)"
                (not (endpointAvailable @"2" theContract tag)
                    .&&. not (endpointAvailable @"1" theContract tag))
                (activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" w1 hdl 1 >> callEndpoint @"2" w1 hdl 2)

        , let theContract :: Contract Schema ContractError () = void $ submitTx mempty >> watchAddressUntil someAddress 20
          in run 1 "submit tx"
                (waitingForSlot theContract tag 20)
                (void $ activateContract w1 theContract tag)

        , let smallTx = Constraints.mustPayToPubKey (Crypto.pubKeyHash $ walletPubKey (Wallet 2)) (Ada.lovelaceValueOf 10)
              theContract :: Contract Schema ContractError () = submitTx smallTx >>= awaitTxConfirmed . Ledger.txId >> submitTx smallTx >>= awaitTxConfirmed . Ledger.txId
          in run 3 "handle several blockchain events"
                (walletFundsChange w1 (Ada.lovelaceValueOf (-20))
                    .&&. assertNoFailedTransactions
                    .&&. assertDone theContract tag (const True) "all blockchain events should be processed")
                (void $ activateContract w1 theContract tag)

        , let l = endpoint @"1" >> endpoint @"2"
              r = endpoint @"3" >> endpoint @"4"
              theContract :: Contract Schema ContractError () = void $ selectEither l r
          in run 1 "select either"
                (assertDone theContract tag (const True) "left branch should finish")
                (activateContract w1 theContract tag >>= (\hdl -> callEndpoint @"1" w1 hdl 1 >> callEndpoint @"2" w1 hdl 2))

        , let theContract :: Contract Schema ContractError () = void $ loopM (\_ -> Left <$> endpoint @"1" @Int) 0
          in run 1 "loopM"
                (endpointAvailable @"1" theContract tag)
                (void $ activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" w1 hdl 1)

        , let theContract :: Contract Schema ContractError () = void $ collectUntil (+) 0 (endpoint @"1") 10
          in run 1 "collect until"
                (endpointAvailable @"1" theContract tag
                    .&&. waitingForSlot theContract tag 10)
                (activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" w1 hdl 1)

        , let theContract :: Contract Schema ContractError () = void $ throwing Con._ContractError $ OtherError "error"
          in run 1 "throw an error"
                (assertContractError theContract tag (\case { OtherError "error" -> True; _ -> False}) "failed to throw error")
                (void $ activateContract w1 theContract tag)
        
        , run 2 "pay to wallet"
            (walletFundsChange w1 (Ada.lovelaceValueOf (-20))
                .&&. walletFundsChange w2 (Ada.lovelaceValueOf 20)
                .&&. assertNoFailedTransactions)
            (void $ Trace.payToWallet w1 w2 (Ada.lovelaceValueOf 20))

        , let theContract :: Contract Schema ContractError PubKey = ownPubKey
          in run 1 "own public key"
                (assertDone theContract tag (== (walletPubKey w2)) "should return the wallet's public key")
                (void $ activateContract w2 (void theContract) tag)

        , let payment = Constraints.mustPayToPubKey (Crypto.pubKeyHash $ walletPubKey w2) (Ada.lovelaceValueOf 10)
              theContract :: Contract Schema ContractError () = submitTx payment >>= awaitTxConfirmed . Ledger.txId
          in run 2 "await tx confirmed"
            (assertDone theContract tag (const True) "should be done")
            (void $ activateContract w1 theContract tag)

        , run 1 "checkpoints"
            (not (endpointAvailable @"2" checkpointContract tag) .&&. (endpointAvailable @"1" checkpointContract tag))
            (void $ activateContract w1 checkpointContract tag >>= \hdl -> callEndpoint @"1" w1 hdl 1 >> callEndpoint @"2" w1 hdl 1)

        , run 1 "error handling & checkpoints"
            (assertDone errorContract tag (\i -> i == 11) "should finish")
            (void $ activateContract w1 (void errorContract) tag >>= \hdl -> callEndpoint @"1" w1 hdl 1 >> callEndpoint @"2" w1 hdl 10 >> callEndpoint @"3" w1 hdl 11)
        ]

w1 :: EM.Wallet
w1 = EM.Wallet 1

w2 :: EM.Wallet
w2 = EM.Wallet 2

checkpointContract :: Contract Schema ContractError ()
checkpointContract = void $ do
    checkpoint $ do
        endpoint @"1" @Int
        endpoint @"2" @Int
    checkpoint $ do
        endpoint @"1" @Int
        endpoint @"3" @Int

errorContract :: Contract Schema ContractError Int
errorContract = do
    catchError
        (endpoint @"1" @Int >> throwError (OtherError "something went wrong"))
        (\_ -> do { checkpoint $ endpoint @"2" @Int; endpoint @"3" @Int })

someAddress :: Address
someAddress = Ledger.scriptAddress $
    Ledger.mkValidatorScript $$(PlutusTx.compile [|| \(_ :: PlutusTx.Data) (_ :: PlutusTx.Data) (_ :: PlutusTx.Data) -> () ||])

type Schema =
    BlockchainActions
        .\/ Endpoint "1" Int
        .\/ Endpoint "2" Int
        .\/ Endpoint "3" Int
        .\/ Endpoint "4" Int
        .\/ Endpoint "ep" ()
