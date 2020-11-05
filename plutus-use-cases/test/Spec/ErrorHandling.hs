{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
module Spec.ErrorHandling(tests) where

import           Language.Plutus.Contract.Test

import           Language.PlutusTx.Coordination.Contracts.ErrorHandling
import qualified Plutus.Trace.Emulator    as Trace

import           Test.Tasty

tests :: TestTree
tests = testGroup "error handling"
    [ checkPredicate
        defaultCheckOptions
        "throw an error"
        (assertContractError contract (Trace.walletInstanceTag w1) (\case { Error1 _ -> True; _ -> False}) "should throw error")
        $ do
            hdl <- Trace.activateContractWallet @_ @MyError w1 contract
            Trace.callEndpoint @"throwError" w1 hdl ()

    , checkPredicate
        defaultCheckOptions
        "catch an error"
        (assertDone @_ @MyError contract (Trace.walletInstanceTag w1) (const True) "should be done")
        $ do
            hdl <- Trace.activateContractWallet @_ @MyError w1 contract
            Trace.callEndpoint @"catchError" w1 hdl ()

    ]

w1 :: Wallet
w1 = Wallet 1
