{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Emulator(
    Emulator
    , ContractHandle(..)
    -- * Interpreter
    , interpretEmulator
    -- * Instance IDs
    , ContractInstanceIdEff(..)
    , uniqueId
    ) where

import           Control.Monad                                   (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error                       (Error)
import           Control.Monad.Freer.Reader                      (runReader)
import           Control.Monad.Freer.State                       (State)
import           Control.Monad.Freer.TH                          (makeEffect)
import qualified Data.Aeson                                      as JSON
import           Data.Proxy                                      (Proxy)
import           Language.Plutus.Contract                        (Contract, HasEndpoint)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoint
import           Ledger.Value                                    (Value)
import           Plutus.Trace.Scheduler                          (Priority (..), SuspendedThread, SysCall (..),
                                                                  ThreadId, mkSysCall, mkThread)
import           Wallet.API                                      (defaultSlotRange, payToPublicKey_)
import qualified Wallet.Emulator                                 as EM
import           Wallet.Emulator.MultiAgent                      (MultiAgentEffect, walletAction)
import           Wallet.Emulator.Wallet                          (Wallet (..))
import           Wallet.Types                                    (ContractInstanceId)

import           Plutus.Trace.Emulator.ContractInstance          (ContractInstanceError, contractThread, getThread)
import           Plutus.Trace.Emulator.Types                     (ContractConstraints, ContractHandle (..), Emulator,
                                                                  EmulatorEvent (..), EmulatorGlobal (..),
                                                                  EmulatorLocal (..), EmulatorState)
import           Plutus.Trace.Types

data ContractInstanceIdEff r where
    UniqueId :: ContractInstanceIdEff ContractInstanceId
makeEffect ''ContractInstanceIdEff

interpretEmulator :: forall effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    )
    => SimulatorInterpreter Emulator effs EmulatorEvent
interpretEmulator = SimulatorInterpreter
    { _runLocal = emRunLocal
    , _runGlobal = emRunGlobal
    }

emRunLocal :: forall b effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    )
    => Wallet
    -> EmulatorLocal b
    -> Eff effs (b, ThreadId -> SuspendedThread effs EmulatorEvent)
emRunLocal wllt = \case
    ActivateContract con -> activate wllt con
    CallEndpointEm p h v -> callEndpoint p h v
    PayToWallet target vl -> payToWallet wllt target vl

payToWallet ::
    ( Member MultiAgentEffect effs )
    => Wallet
    -> Wallet
    -> Value
    -> Eff effs ((), ThreadId -> SuspendedThread effs EmulatorEvent)
payToWallet source target amount = do
    let payment = walletAction source $ payToPublicKey_ defaultSlotRange amount (EM.walletPubKey target)
    pure ((), mkThread High payment)

activate ::
    ( ContractConstraints s
    , Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    )
    => Wallet
    -> Contract s e ()
    -> Eff effs (ContractHandle s e, ThreadId -> SuspendedThread effs EmulatorEvent)
activate wllt con = do
    i <- uniqueId
    let handle = ContractHandle{chContract=con, chInstanceId = i}
    pure (handle, mkThread High (runReader wllt $ contractThread handle))

callEndpoint :: forall s l e ep effs.
    ( ContractConstraints s
    , HasEndpoint l ep s
    , Member (State EmulatorState) effs
    , Member (Error ContractInstanceError) effs
    )
    => Proxy l
    -> ContractHandle s e
    -> ep
    -> Eff effs ((), ThreadId -> SuspendedThread effs EmulatorEvent)
callEndpoint _ ContractHandle{chInstanceId} ep = do
    threadId <- getThread chInstanceId
    let epJson = JSON.toJSON $ Endpoint.event @l @ep @s ep
        thr = void $ mkSysCall @effs @EmulatorEvent High (Message threadId $ EndpointCall epJson)
    pure ((), mkThread High thr)

emRunGlobal :: forall b effs. EmulatorGlobal b -> Eff effs (b, ThreadId -> SuspendedThread effs EmulatorEvent)
emRunGlobal = \case
    _ -> undefined

