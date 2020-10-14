{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
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
    , EmulatorThreads(..)
    , instanceIdThreads
    , EmulatorAgentThreadEffs
    , ContractHandle(..)
    , Emulator
    , EmulatorLocal(..)
    , EmulatorGlobal(..)
    , ContractConstraints
    -- * Constructing Traces
    , activateContract
    , callEndpoint
    , payToWallet
    , waitUntilSlot
    ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Reader      (Reader)
import qualified Data.Aeson                      as JSON
import           Data.Map                        (Map)
import           Data.Proxy                      (Proxy (..))
import qualified Data.Row.Internal               as V
import           Language.Plutus.Contract        (Contract, HasBlockchainActions, HasEndpoint)
import           Language.Plutus.Contract.Schema (Input, Output)
import           Ledger.Slot                     (Slot)
import           Ledger.Tx                       (Tx)
import           Ledger.Value                    (Value)
import           Plutus.Trace.Scheduler          (SystemCall, ThreadId)
import           Plutus.Trace.Types              (Trace (..), TraceBackend (..))
import           Wallet.Emulator.Wallet          (Wallet (..))
import           Wallet.Types                    (ContractInstanceId, Notification)
import Wallet.Emulator.SigningProcess (SigningProcess)

type ContractConstraints s =
    ( V.Forall (Output s) V.Unconstrained1
    , V.Forall (Input s) V.Unconstrained1
    , V.AllUniqueLabels (Input s)
    , V.Forall (Input s) JSON.FromJSON
    , V.Forall (Input s) JSON.ToJSON
    , HasBlockchainActions s
    )

data EmulatorEvent =
    BlockAdded [Tx]
    | NewSlot Slot
    | EndpointCall JSON.Value
    | Notify Notification
    deriving stock (Eq, Show)

-- | A map of contract instance ID to thread ID
newtype EmulatorThreads =
    EmulatorThreads
        { _instanceIdThreads :: Map ContractInstanceId ThreadId
        } deriving newtype (Semigroup, Monoid)

makeLenses ''EmulatorThreads

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
    ActivateContract :: ContractConstraints s => Contract s e () -> EmulatorLocal (ContractHandle s e)
    CallEndpointEm :: forall l ep s e. (ContractConstraints s, HasEndpoint l ep s) => Proxy l -> ContractHandle s e -> ep -> EmulatorLocal ()
    PayToWallet :: Wallet -> Value -> EmulatorLocal ()
    SetSigningProcess :: SigningProcess -> EmulatorLocal ()

data EmulatorGlobal r where
    WaitUntilSlot :: Slot -> EmulatorGlobal Slot

instance TraceBackend Emulator where
    type LocalAction Emulator = EmulatorLocal
    type GlobalAction Emulator = EmulatorGlobal
    type Agent Emulator = Wallet

type EmulatorTrace a = Eff '[Trace Emulator] a

activateContract :: forall s e. ContractConstraints s => Wallet -> Contract s e () -> EmulatorTrace (ContractHandle s e)
activateContract wallet = send @(Trace Emulator) . RunLocal wallet . ActivateContract

callEndpoint :: forall l ep s e. (ContractConstraints s, HasEndpoint l ep s) => Wallet -> ContractHandle s e -> ep -> EmulatorTrace ()
callEndpoint wallet hdl = send @(Trace Emulator) . RunLocal wallet . CallEndpointEm (Proxy @l) hdl

payToWallet :: Wallet -> Wallet -> Value -> EmulatorTrace ()
payToWallet from_ to_ = send @(Trace Emulator) . RunLocal from_ . PayToWallet to_

waitUntilSlot :: Slot -> EmulatorTrace Slot
waitUntilSlot sl = send @(Trace Emulator) $ RunGlobal (WaitUntilSlot sl)
