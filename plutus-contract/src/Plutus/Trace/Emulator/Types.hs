{-# LANGUAGE OverloadedStrings #-}
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
    , walletInstanceTag
    , ContractHandle(..)
    , Emulator
    , EmulatorLocal(..)
    , EmulatorGlobal(..)
    , ContractConstraints
    -- * Constructing Traces
    , activateContract
    , activateContractWallet
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
    , _CurrentRequests
    , _InstErr
    , _ContractLog
    ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Log            (LogMsg)
import           Control.Monad.Freer.Reader         (Reader)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Aeson                         as JSON
import           Data.Map                           (Map)
import Data.String (IsString(..))
import           Data.Proxy                         (Proxy (..))
import           Data.Text.Prettyprint.Doc (Pretty(..), (<+>), colon, braces, viaShow, fillSep, vsep, hang)
import qualified Data.Row.Internal                  as V
import           GHC.Generics                       (Generic)
import Data.Text (Text)
import           Language.Plutus.Contract           (Contract, HasBlockchainActions, HasEndpoint)
import           Language.Plutus.Contract.Resumable (Request (..), Response (..))
import           Language.Plutus.Contract.Schema    (Input, Output)
import Ledger.Interval (Interval)
import qualified Ledger.Interval as I
import           Ledger.Slot                        (Slot (..))
import           Ledger.Tx                          (Tx)
import Ledger.TxId (TxId)
import           Ledger.Value                       (Value)
import           Numeric.Natural                    (Natural)
import           Plutus.Trace.Scheduler             (SystemCall, ThreadId)
import           Plutus.Trace.Types                 (Trace (..), TraceBackend (..))
import           Wallet.Emulator.Chain              (ChainState)
import qualified Wallet.Emulator.Chain              as ChainState
import           Wallet.Emulator.Wallet             (SigningProcess, Wallet (..), WalletState)
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
    ActivateContract :: (ContractConstraints s, HasBlockchainActions s) => Contract s e () -> ContractInstanceTag -> EmulatorLocal (ContractHandle s e)
    CallEndpointEm :: forall l ep s e. (ContractConstraints s, HasEndpoint l ep s) => Proxy l -> ContractHandle s e -> ep -> EmulatorLocal ()
    PayToWallet :: Interval Slot -> Wallet -> Value -> EmulatorLocal TxId
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

activateContract :: forall s e. (HasBlockchainActions s, ContractConstraints s) => Wallet -> Contract s e () -> ContractInstanceTag -> EmulatorTrace (ContractHandle s e)
activateContract wallet contract = send @(Trace Emulator) . RunLocal wallet . ActivateContract contract

activateContractWallet :: forall s e. (HasBlockchainActions s, ContractConstraints s) => Wallet -> Contract s e () -> EmulatorTrace (ContractHandle s e)
activateContractWallet w contract = activateContract w contract (walletInstanceTag w)

callEndpoint :: forall l ep s e. (ContractConstraints s, HasEndpoint l ep s) => Wallet -> ContractHandle s e -> ep -> EmulatorTrace ()
callEndpoint wallet hdl = send @(Trace Emulator) . RunLocal wallet . CallEndpointEm (Proxy @l) hdl

-- | Pay some funds from one wallet to another
payToWallet :: Wallet -> Wallet -> Value -> EmulatorTrace TxId
payToWallet from_ to_ = payToWallet' I.always from_ to_

-- | Pay some funds from one wallet to another with the given validity range
payToWallet' :: Interval Slot -> Wallet -> Wallet -> Value -> EmulatorTrace TxId
payToWallet' range_ from_ to_ = send @(Trace Emulator) . RunLocal from_ . PayToWallet range_ to_

-- | Wait until the simulation has reached the given slot
waitUntilSlot :: Slot -> EmulatorTrace Slot
waitUntilSlot sl = send @(Trace Emulator) $ RunGlobal (WaitUntilSlot sl)

-- | Look at the 'WalletState' of the agent
agentState :: Wallet -> EmulatorTrace WalletState
agentState wallet = send @(Trace Emulator) $ RunLocal wallet AgentState

-- | Look at the 'ChainState'
chainState :: EmulatorTrace ChainState
chainState = send @(Trace Emulator) $ RunGlobal ChainState

-- | Wait for a number of slots
waitNSlots :: Natural -> EmulatorTrace Slot
waitNSlots n = do
    Slot c <- view ChainState.currentSlot <$> chainState
    waitUntilSlot (Slot $ c + fromIntegral n)

data ContractInstanceError =
    ThreadIdNotFound ContractInstanceId
    | InstanceIdNotFound Wallet
    | JSONDecodingError String
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty ContractInstanceError where
    pretty = \case
        ThreadIdNotFound i -> "Thread ID not found:" <+> pretty i
        InstanceIdNotFound w -> "Instance ID not found:" <+> pretty w
        JSONDecodingError e -> "JSON decoding error:" <+> pretty e

-- | A user-defined tag for a contract instance. Used to find the instance's
--   log messages in the emulator log.
newtype ContractInstanceTag = ContractInstanceTag { unContractInstanceTag :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (IsString, Pretty)

-- | The 'ContractInstanceTag' for the contract instance of a wallet. See note 
--   [Wallet contract instances]
walletInstanceTag :: Wallet -> ContractInstanceTag
walletInstanceTag wllt = fromString $ "Contract instance for " <> show wllt

data ContractInstanceMsg =
    Started
    | Stopped
    | ReceiveEndpointCall JSON.Value
    | NoRequestsHandled
    | HandledRequest (Response JSON.Value)
    | CurrentRequests [Request JSON.Value]
    | InstErr ContractInstanceError
    | ContractLog JSON.Value
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty ContractInstanceMsg where
    pretty = \case
        Started -> "Started"
        Stopped -> "Stopped"
        ReceiveEndpointCall v -> "Receive endpoint call:" <+> viaShow v
        NoRequestsHandled -> "No requests handled"
        HandledRequest rsp -> "Handled request:" <+> pretty (show . JSON.encode <$> rsp)
        CurrentRequests lst -> "Current requests:" <+> fillSep (pretty . fmap (show . JSON.encode) <$> lst)
        InstErr e -> "Error:" <+> pretty e
        ContractLog v -> "Contract log:" <+> viaShow v

data ContractInstanceLog =
    ContractInstanceLog
        { _cilMessage :: ContractInstanceMsg
        , _cilId      :: ContractInstanceId
        , _cilTag     :: ContractInstanceTag
        }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty ContractInstanceLog where
    pretty ContractInstanceLog{_cilMessage, _cilId, _cilTag} =
        hang 2 $ vsep [pretty _cilId <+> braces (pretty _cilTag) <> colon, pretty _cilMessage]

makeLenses ''ContractInstanceLog
makePrisms ''ContractInstanceMsg