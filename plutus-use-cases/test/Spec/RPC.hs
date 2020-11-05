{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
module Spec.RPC(tests) where

import Control.Monad (void)
import           Data.Either                                  (isRight)
import           Language.Plutus.Contract
import           Language.Plutus.Contract.Test
import           Language.PlutusTx.Lattice
import           Wallet.Emulator.Notify                       (walletInstanceId)
import qualified Plutus.Trace.Emulator    as Trace
import Plutus.Trace.Emulator (EmulatorTrace)

import           Language.PlutusTx.Coordination.Contracts.RPC

import           Test.Tasty

theContract :: Contract AdderSchema AdderError (Either (Either CancelRPC Integer) ())
theContract = callAdder `selectEither` respondAdder

cancelContract :: Contract AdderSchema AdderError (Either (Either CancelRPC Integer) ())
cancelContract = callAdderCancel `selectEither` respondAdder

server, client :: Wallet
server = Wallet 1
client = Wallet 2

tests :: TestTree
tests = testGroup "RPC"
    [ checkPredicate defaultCheckOptions "call RPC"
        (assertDone theContract (Trace.walletInstanceTag server) isRight ""
        .&&. assertDone theContract (Trace.walletInstanceTag client) (\case { Left (Right 4) -> True; _ -> False}) "")
        $ do
            shdl <- Trace.activateContractWallet server (void theContract)
            chdl <- Trace.activateContractWallet client (void theContract)
            Trace.callEndpoint @"serve" server shdl ()
            Trace.callEndpoint @"target instance" client chdl (Trace.chInstanceId shdl)

    , checkPredicate defaultCheckOptions "call RPC with error"
        (assertDone cancelContract (Trace.walletInstanceTag server) isRight ""
        .&&. assertDone cancelContract (Trace.walletInstanceTag client) (\case { Left (Left CancelRPC) -> True; _ -> False}) "")
        $ do
            (shdl, chdl) <- 
                (,) <$> Trace.activateContractWallet server (void cancelContract)
                    <*> Trace.activateContractWallet client (void cancelContract)
            Trace.callEndpoint @"serve" server shdl ()
            Trace.callEndpoint @"target instance" client chdl (Trace.chInstanceId shdl)
    ]
