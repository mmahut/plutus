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
    , activateContract
    , callEndpoint
    , payToWallet
    , waitUntilSlot
    , etrace
    ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Reader     (Reader, ask, runReader)
import           Control.Monad.Freer.State      (State, modify)
import           Control.Monad.Freer.TH         (makeEffect)
import qualified Data.Aeson                     as JSON
import           Data.Map                       (Map)
import           Data.Proxy                     (Proxy (..))
import           Data.Void                      (Void)
import           Language.Plutus.Contract       (type (.\/), BlockchainActions, Contract, Endpoint, HasEndpoint)
import           Language.Plutus.Contract.Trace (handleBlockchainQueries)
import           Ledger.Slot                    (Slot)
import           Ledger.Value                   (Value)
import           Plutus.Trace.Scheduler         (Priority (..), SimulatorSystemCall (..), SuspendedThread, SysCall (..),
                                                 ThreadId, mkSysCall, mkThread)
import           Wallet.Effects
import           Wallet.Emulator.Wallet         (Wallet (..))
import           Wallet.Types                   (ContractInstanceId)

import           Plutus.Trace.Types

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

data EmulatorEvent =
    BlockAdded -- [Tx]
    | NewSlot -- Slot
    | EndpointCall String JSON.Value

data ContractInstanceIdEff r where
    UniqueId :: ContractInstanceIdEff ContractInstanceId
makeEffect ''ContractInstanceIdEff

data EmulatorState =
    EmulatorState
        { _instanceIdThreads :: Map ContractInstanceId ThreadId
        }

makeLenses ''EmulatorState

i :: forall effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    )
    => SimulatorInterpreter Emulator effs EmulatorEvent
i = SimulatorInterpreter
    { _runLocal = emRunLocal
    , _runGlobal = emRunGlobal
    }

emRunLocal :: forall b effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    )
    => Wallet
    -> EmulatorLocal b
    -> Eff effs (b, ThreadId -> SuspendedThread effs EmulatorEvent)
emRunLocal wllt = \case
    ActivateContract con -> do
        i <- uniqueId
        let handle = ContractHandle{chContract=con, chInstanceId = i}
        pure (handle, mkThread High (runReader wllt $ contractThread handle))

-- | Effects available to threads that run in the context of specific
--   agents (ie wallets)
type EmulatorAgentThreadEffs effs =
    Reader Wallet
    ': Reader ThreadId
    ': Yield (SimulatorSystemCall effs EmulatorEvent) (Maybe EmulatorEvent)
    ': effs

contractThread :: forall s e effs.
    ( Member (State EmulatorState) effs
    )
    => ContractHandle s e
    -> Eff (EmulatorAgentThreadEffs effs) ()
contractThread ContractHandle{chInstanceId, chContract} = do
    ask @ThreadId >>= registerInstance chInstanceId
    mkSysCall @effs @EmulatorEvent Low Suspend >>= runInstance chContract

registerInstance :: forall effs.
    ( Member (State EmulatorState) effs )
    => ContractInstanceId
    -> ThreadId
    -> Eff effs ()
registerInstance i t = modify (instanceIdThreads . at i .~ Just t)

runInstance :: forall s e effs.
    Contract s e ()
    -> Maybe EmulatorEvent
    -> Eff (EmulatorAgentThreadEffs effs) ()
runInstance contract event = do
    case event of
        Just (EndpointCall ep vl) -> do
            -- handle the endpoint
            pure ()
        _ -> do
            -- see if we can handle any requests
            mkSysCall @effs @EmulatorEvent Low Suspend >>= runInstance contract

emRunGlobal :: forall b effs. EmulatorGlobal b -> Eff effs (b, ThreadId -> SuspendedThread effs EmulatorEvent)
emRunGlobal = undefined
