{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
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
    EmulatorMessage(..)
    , EmulatorThreads(..)
    , instanceIdThreads
    , EmulatorAgentThreadEffs
    , ContractInstanceTag(..)
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
    , waitNSlots
    , agentState
    , chainState
    -- * Logging
    , ContractInstanceLog(..)
    , cilId
    , cilMessage
    , cilTag
    , ContractInstanceError(..)
    , ContractInstanceMsg(..)
    , _Started
    , _Stopped
    , _ReceiveEndpointCall
    , _NoRequestsHandled
    , _HandledRequest
    , _HandleInstanceRequests
    , _InstErr
    ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Log            (LogMsg)
import           Control.Monad.Freer.Reader         (Reader)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Aeson                         as JSON
import           Data.Map                           (Map)
import Data.String (IsString)
import           Data.Proxy                         (Proxy (..))
import           Data.Text.Prettyprint.Doc (Pretty)
import qualified Data.Row.Internal                  as V
import           GHC.Generics                       (Generic)
import Data.Text (Text)
import           Language.Plutus.Contract           (Contract, HasBlockchainActions, HasEndpoint)
import           Language.Plutus.Contract.Resumable (Request (..), Requests (..), Response (..))
import           Language.Plutus.Contract.Schema    (Input, Output)
import           Ledger.Slot                        (Slot (..))
import           Ledger.Tx                          (Tx)
import           Ledger.Value                       (Value)
import           Numeric.Natural                    (Natural)
import           Plutus.Trace.Scheduler             (SystemCall, ThreadId)
import           Plutus.Trace.Types                 (Trace (..), TraceBackend (..))
import           Wallet.Emulator.Chain              (ChainState)
import qualified Wallet.Emulator.Chain              as ChainState
import           Wallet.Emulator.Wallet             (SigningProcess, Wallet (..), WalletState)
import qualified Wallet.Emulator.Wallet             as Wallet
import           Wallet.Types                       (ContractInstanceId, Notification)

type ContractConstraints s =
    ( V.Forall (Output s) V.Unconstrained1
    , V.Forall (Input s) V.Unconstrained1
    , V.AllUniqueLabels (Input s)
    , V.Forall (Input s) JSON.FromJSON
    , V.Forall (Input s) JSON.ToJSON
    , V.Forall (Output s) JSON.FromJSON
    , V.Forall (Output s) JSON.ToJSON
    )

data EmulatorMessage =
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
    LogMsg ContractInstanceLog
    ': Reader Wallet
    ': Reader ThreadId
    ': Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage)
    ': effs

data Emulator

-- | A reference to a running contract in the emulator.
data ContractHandle s e =
    ContractHandle
        { chContract    :: Contract s e ()
        , chInstanceId  :: ContractInstanceId
        , chInstanceTag :: ContractInstanceTag
        }

data EmulatorLocal r where
    ActivateContract :: (ContractConstraints s, HasBlockchainActions s) => ContractInstanceTag -> Contract s e () -> EmulatorLocal (ContractHandle s e)
    CallEndpointEm :: forall l ep s e. (ContractConstraints s, HasEndpoint l ep s) => Proxy l -> ContractHandle s e -> ep -> EmulatorLocal ()
    PayToWallet :: Wallet -> Value -> EmulatorLocal ()
    SetSigningProcess :: SigningProcess -> EmulatorLocal ()
    AgentState :: EmulatorLocal WalletState

data EmulatorGlobal r where
    WaitUntilSlot :: Slot -> EmulatorGlobal Slot
    ChainState :: EmulatorGlobal ChainState

instance TraceBackend Emulator where
    type LocalAction Emulator = EmulatorLocal
    type GlobalAction Emulator = EmulatorGlobal
    type Agent Emulator = Wallet

type EmulatorTrace a = Eff '[Trace Emulator] a

activateContract :: forall s e. (HasBlockchainActions s, ContractConstraints s) => Wallet -> ContractInstanceTag -> Contract s e () -> EmulatorTrace (ContractHandle s e)
activateContract wallet tag = send @(Trace Emulator) . RunLocal wallet . ActivateContract tag

callEndpoint :: forall l ep s e. (ContractConstraints s, HasEndpoint l ep s) => Wallet -> ContractHandle s e -> ep -> EmulatorTrace ()
callEndpoint wallet hdl = send @(Trace Emulator) . RunLocal wallet . CallEndpointEm (Proxy @l) hdl

payToWallet :: Wallet -> Wallet -> Value -> EmulatorTrace ()
payToWallet from_ to_ = send @(Trace Emulator) . RunLocal from_ . PayToWallet to_

waitUntilSlot :: Slot -> EmulatorTrace Slot
waitUntilSlot sl = send @(Trace Emulator) $ RunGlobal (WaitUntilSlot sl)

agentState :: Wallet -> EmulatorTrace WalletState
agentState wallet = send @(Trace Emulator) $ RunLocal wallet AgentState

chainState :: EmulatorTrace ChainState
chainState = send @(Trace Emulator) $ RunGlobal ChainState

waitNSlots :: Natural -> EmulatorTrace Slot
waitNSlots n = do
    Slot c <- view ChainState.currentSlot <$> chainState
    waitUntilSlot (Slot $ c + fromIntegral n)

data ContractInstanceError =
    ThreadIdNotFound ContractInstanceId
    | JSONDecodingError String
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A user-defined tag for a contract instance. Used to find the instance's
--   log messages in the emulator log.
newtype ContractInstanceTag = ContractInstanceTag { unContractInstanceTag :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (IsString, Pretty)

data ContractInstanceMsg =
    Started
    | Stopped
    | ReceiveEndpointCall JSON.Value
    | NoRequestsHandled
    | HandledRequest (Response JSON.Value)
    | HandleInstanceRequests [Request JSON.Value]
    | InstErr ContractInstanceError
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ContractInstanceLog =
    ContractInstanceLog
        { _cilMessage :: ContractInstanceMsg
        , _cilId      :: ContractInstanceId
        , _cilTag     :: ContractInstanceTag
        }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeLenses ''ContractInstanceLog
makePrisms ''ContractInstanceMsg