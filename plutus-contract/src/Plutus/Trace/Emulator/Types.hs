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

module Plutus.Trace.Emulator.Types(
    EmulatorEvent(..)
    , EmulatorState(..)
    , instanceIdThreads
    , EmulatorAgentThreadEffs
    , ContractHandle(..)
    ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Reader    (Reader)
import qualified Data.Aeson                    as JSON
import           Data.Map                      (Map)
import           Data.Proxy                    (Proxy (..))
import           Data.Void                     (Void)
import           Language.Plutus.Contract      (type (.\/), BlockchainActions, Contract, Endpoint, HasEndpoint)
import           Ledger.Slot                   (Slot)
import           Ledger.Value                  (Value)
import           Plutus.Trace.Scheduler        (Priority (..), SuspendedThread, SysCall (..), SystemCall (..), ThreadId,
                                                mkSysCall, mkThread)
import           Plutus.Trace.Types            (Simulator (..), SimulatorBackend (..))
import           Wallet.Emulator.Wallet        (Wallet (..))
import           Wallet.Types                  (ContractInstanceId)

data EmulatorEvent =
    BlockAdded -- [Tx]
    | NewSlot -- Slot
    | EndpointCall String JSON.Value

data EmulatorState =
    EmulatorState
        { _instanceIdThreads :: Map ContractInstanceId ThreadId
        }

makeLenses ''EmulatorState

type EmulatorAgentThreadEffs effs =
    Reader Wallet
    ': Reader ThreadId
    ': Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent)
    ': effs


data Emulator

-- | A reference to an installed contract in the emulator.
data ContractHandle s e =
    ContractHandle
        { chContract   :: Contract s e ()
        , chInstanceId :: ContractInstanceId
        }

data EmulatorLocal r where
    ActivateContract :: Contract s e () -> EmulatorLocal (ContractHandle s e)
    CallEndpointEm :: forall l ep s e. HasEndpoint l ep s => Proxy l -> ContractHandle s e -> ep -> EmulatorLocal ()
    PayToWallet :: Wallet -> Value -> EmulatorLocal ()

data EmulatorGlobal r where
    WaitUntilSlot :: Slot -> EmulatorGlobal ()

instance SimulatorBackend Emulator where
    type LocalAction Emulator = EmulatorLocal
    type GlobalAction Emulator = EmulatorGlobal
    type Agent Emulator = Wallet

type EmulatorTrace a = Eff '[Simulator Emulator] a

activateContract :: forall s e. Wallet -> Contract s e () -> EmulatorTrace (ContractHandle s e)
activateContract wallet = send @(Simulator Emulator) . RunLocal wallet . ActivateContract

callEndpoint :: forall l ep s e. HasEndpoint l ep s => Wallet -> ContractHandle s e -> ep -> EmulatorTrace ()
callEndpoint wallet hdl = send @(Simulator Emulator) . RunLocal wallet . CallEndpointEm (Proxy @l) hdl

payToWallet :: Wallet -> Wallet -> Value -> EmulatorTrace ()
payToWallet from_ to_ = send @(Simulator Emulator) . RunLocal from_ . PayToWallet to_

waitUntilSlot :: Slot -> EmulatorTrace ()
waitUntilSlot sl = send @(Simulator Emulator) $ RunGlobal (WaitUntilSlot sl)

myContract :: Contract (BlockchainActions .\/ Endpoint "my endpoint" Int) Void ()
myContract = undefined

etrace :: EmulatorTrace ()
etrace = do
    runningCon <- activateContract (Wallet 1) myContract
    callEndpoint @"my endpoint" (Wallet 1) runningCon 10
