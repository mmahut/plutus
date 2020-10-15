{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | A trace is a sequence of actions by simulated wallets that can be run
--   on the mockchain. This module contains the functions needed to build
--   traces.
module Language.Plutus.Contract.Trace
    ( ContractTraceState
    , TraceError(..)
    , EndpointError(..)
    , AsTraceError(..)
    , toNotifyError
    , WalletState(..)
    , ctsWalletStates
    , ctsContract
    , eventsByWallet
    , handlersByWallet
    , checkpointStoreByWallet
    , ContractTraceResult(..)
    , ctrEmulatorState
    , ctrTraceState
    -- * Running 'ContractTrace' actions
    -- * Constructing 'ContractTrace' actions
    , handleUtxoQueries
    , handleNextTxAtQueries
    , addNamedEvent
    , addResponse
    -- * Handle blockchain events repeatedly
    , handleBlockchainQueries
    , handleSlotNotifications
    -- * Initial distributions of emulated chains
    , InitialDistribution
    , defaultDist
    , defaultDistFor
    -- * Wallets
    , EM.Wallet(..)
    , EM.walletPubKey
    , EM.walletPrivKey
    , allWallets
    , makeTimed
    ) where

import           Control.Arrow                                     ((>>>), (>>^))
import           Control.Lens                                      (from, makeClassyPrisms, makeLenses, review, use,
                                                                    view, (%=))
import           Control.Monad                                     (guard)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error                         (Error, throwError)
import qualified Control.Monad.Freer.Extras                        as Eff
import           Control.Monad.Freer.Log                           (LogMessage, LogMsg, LogObserve)
import           Control.Monad.Freer.Reader                        (Reader)
import           Control.Monad.Freer.State                         (State, gets)
import qualified Data.Aeson.Types                                  as JSON
import           Data.Foldable                                     (toList)
import           Data.Map                                          (Map)
import qualified Data.Map                                          as Map
import           Data.Maybe                                        (fromMaybe, mapMaybe)
import qualified Data.Row.Internal                                 as V
import qualified Data.Row.Variants                                 as V
import           Data.Sequence                                     (Seq, (|>))
import           Data.Text.Prettyprint.Doc                         (Pretty, pretty, (<+>))
import           GHC.Generics                                      (Generic)

import           Data.Text                                         (Text)
import           Language.Plutus.Contract                          (Contract (..), HasAwaitSlot, HasTxConfirmation,
                                                                    HasUtxoAt, HasWatchAddress, HasWriteTx)
import           Language.Plutus.Contract.Checkpoint               (CheckpointStore)
import qualified Language.Plutus.Contract.Resumable                as State
import           Language.Plutus.Contract.Schema                   (Event (..), Handlers (..), Output)
import qualified Language.Plutus.Contract.Types                    as Contract.Types

import qualified Language.Plutus.Contract.Effects.AwaitSlot        as AwaitSlot
import           Language.Plutus.Contract.Effects.AwaitTxConfirmed (TxConfirmed (..))
import qualified Language.Plutus.Contract.Effects.AwaitTxConfirmed as AwaitTxConfirmed
import           Language.Plutus.Contract.Effects.Instance         (HasOwnId)
import qualified Language.Plutus.Contract.Effects.Instance         as OwnInstance
import           Language.Plutus.Contract.Effects.Notify           (HasContractNotify)
import qualified Language.Plutus.Contract.Effects.Notify           as Notify
import           Language.Plutus.Contract.Effects.OwnPubKey        (HasOwnPubKey)
import qualified Language.Plutus.Contract.Effects.OwnPubKey        as OwnPubKey
import qualified Language.Plutus.Contract.Effects.UtxoAt           as UtxoAt
import qualified Language.Plutus.Contract.Effects.WatchAddress     as WatchAddress
import qualified Language.Plutus.Contract.Effects.WriteTx          as WriteTx
import           Language.Plutus.Contract.Resumable                (Request (..), Requests (..), Response (..))
import           Language.Plutus.Contract.Trace.RequestHandler     (MaxIterations (..), RequestHandler (..),
                                                                    RequestHandlerLogMsg, maybeToHandler)
import qualified Language.Plutus.Contract.Trace.RequestHandler     as RequestHandler
import           Language.Plutus.Contract.Types                    (ResumableResult (..))

import qualified Ledger.Ada                                        as Ada
import           Ledger.Value                                      (Value)

import           Wallet.API                                        (ChainIndexEffect, SigningProcessEffect)
import           Wallet.Effects                                    (ContractRuntimeEffect, WalletEffect)
import           Wallet.Emulator                                   (EmulatorState, Wallet)
import qualified Wallet.Emulator                                   as EM
import           Wallet.Emulator.LogMessages                       (TxBalanceMsg)
import           Wallet.Emulator.MultiAgent                        (EmulatedWalletEffects)
import qualified Wallet.Emulator.MultiAgent                        as EM
import           Wallet.Emulator.Notify                            (EmulatorNotifyLogMsg (..))
import           Wallet.Types                                      (ContractInstanceId, EndpointDescription (..),
                                                                    NotificationError (..))

data EndpointError =
    EndpointNotActive (Maybe Wallet) EndpointDescription
    | MoreThanOneEndpointActive EndpointDescription
    deriving stock (Eq, Show, Generic)
    deriving anyclass (JSON.ToJSON, JSON.FromJSON)

instance Pretty EndpointError where
    pretty = \case
        EndpointNotActive w e ->
            "Endpoint not active:" <+> pretty w <+> pretty e
        MoreThanOneEndpointActive e ->
            "More than one endpoint active:" <+> pretty e

toNotifyError :: ContractInstanceId -> EndpointError -> NotificationError
toNotifyError i = \case
    EndpointNotActive _ e -> EndpointNotAvailable i e
    MoreThanOneEndpointActive e -> MoreThanOneEndpointAvailable i e

-- | Error produced while running a trace. Either a contract-specific
--   error (of type 'e'), or an 'EM.AssertionError' from the emulator.
data TraceError e =
    TraceAssertionError EM.AssertionError
    | TContractError e
    | HandleBlockchainEventsMaxIterationsExceeded Wallet MaxIterations
    | HookError EndpointError
    deriving (Eq, Show)

type InitialDistribution = Map Wallet Value

data WalletState s e a =
    WalletState
        { walletContractState   :: ResumableResult e (Event s) (Handlers s) a
        , walletEvents          :: Seq (Response (Event s))
        , walletHandlersHistory :: Seq [State.Request (Handlers s)]
        }

walletHandlers :: WalletState s (TraceError e) a -> [State.Request (Handlers s)]
walletHandlers = State.unRequests . wcsRequests . walletContractState

emptyWalletState :: Contract s e a -> WalletState s e a
emptyWalletState (Contract c) =
    WalletState
        { walletContractState = Contract.Types.runResumable [] mempty c
        , walletEvents = mempty
        , walletHandlersHistory = mempty
        }

addEventWalletState ::
    Contract s e a
    -> Response (Event s)
    -> WalletState s e a
    -> WalletState s e a
addEventWalletState (Contract c) event s@WalletState{walletContractState, walletEvents, walletHandlersHistory} =
    let ResumableResult{wcsResponses,wcsRequests=Requests{unRequests},wcsCheckpointStore} = walletContractState
        state' = Contract.Types.insertAndUpdate c wcsCheckpointStore wcsResponses event
        events' = walletEvents |> event
        history' = walletHandlersHistory |> unRequests
    in s { walletContractState = state', walletEvents = events', walletHandlersHistory = history'}

data ContractTraceState s e a =
    ContractTraceState
        { _ctsWalletStates :: Map Wallet (WalletState s e a)
        -- ^ The state of the contract instance (per wallet). To get
        --   the 'Record' of a sequence of events, use
        --   'Language.Plutus.Contract.Resumable.runResumable'.
        , _ctsContract     :: Contract s e a
        -- ^ Current state of the contract
        }

eventsByWallet :: ContractTraceState s e a -> Map Wallet [Response (Event s)]
eventsByWallet = fmap (toList . walletEvents) . _ctsWalletStates

handlersByWallet :: ContractTraceState s e a -> Map Wallet [[State.Request (Handlers s)]]
handlersByWallet = fmap (toList . walletHandlersHistory) . _ctsWalletStates

checkpointStoreByWallet :: ContractTraceState s e a -> Map Wallet CheckpointStore
checkpointStoreByWallet = fmap (wcsCheckpointStore . walletContractState) . _ctsWalletStates

makeLenses ''ContractTraceState

{-| A variant of 'addEvent' that takes the name of the endpoint as a value
    instead of a type argument. This is useful for the playground where we
    don't have the type-level symbol of user-defined endpoint calls.

    Unfortunately this requires the 'V.Forall (Output s) V.Unconstrained1'
    constraint. Luckily 'V.Forall (Output s) V.Unconstrained1' holds for all
    schemas, so it doesn't restrict the callers of 'addNamedEvent'. But we
    have to propagate it up to the top level :(

-}
addNamedEvent ::
    forall s e a effs.
    ( V.Forall (Output s) V.Unconstrained1 -- TODO: remove
    , Member (Error EndpointError) effs
    , Member (State (ContractTraceState s (TraceError e) a)) effs
    )
    => String -- endpoint name
    -> Wallet
    -> Event s
    -> Eff effs ()
addNamedEvent endpointName wallet event = do
    let filterReq Request{rqID, itID, rqRequest=Handlers v} = do
            guard $
                (V.eraseWithLabels @V.Unconstrained1 (const ()) v)
                    == (endpointName, ())
            Just Response{rspRqID=rqID, rspItID=itID, rspResponse=event}
    hks <- mapMaybe filterReq <$> getHooks @s @e @a wallet >>= \case
            [] -> throwError $ EndpointNotActive Nothing $ EndpointDescription endpointName
            [x] -> pure x
            _ -> throwError $ MoreThanOneEndpointActive $ EndpointDescription endpointName
    addResponse @s @e @a wallet hks


-- | Add a 'Response' to the wallet's trace
addResponse
    :: forall s e a effs.
    ( Member (State (ContractTraceState s (TraceError e) a)) effs
    )
    => Wallet
    -> Response (Event s)
    -> Eff effs ()
addResponse w e = Eff.monadStateToState @(ContractTraceState s (TraceError e) a) $ do
    con <- use ctsContract
    let go st =
            let theState = fromMaybe (emptyWalletState con) st
             in Just (addEventWalletState con e theState)
    ctsWalletStates %= Map.alter go w

-- | Get the hooks that a contract is currently waiting for
getHooks
    :: forall s e a effs.
    ( Member (State (ContractTraceState s (TraceError e) a)) effs )
    => Wallet
    -> Eff effs [Request (Handlers s)]
getHooks w =
    foldMap walletHandlers . Map.lookup w <$> gets @(ContractTraceState s (TraceError e) a) (view ctsWalletStates)

data ContractTraceResult s e a =
    ContractTraceResult
        { _ctrEmulatorState :: EmulatorState
        -- ^ The emulator state at the end of the test
        , _ctrTraceState    :: ContractTraceState s e a
        -- ^ Final 'ContractTraceState'
        }

makeLenses ''ContractTraceResult

makeTimed :: Member (State EmulatorState) effs => EmulatorNotifyLogMsg -> Eff effs EM.EmulatorEvent
makeTimed e = do
    emulatorTime <- gets (view (EM.chainState . EM.currentSlot))
    pure $ review (EM.emulatorTimeEvent emulatorTime) (EM.NotificationEvent e)

handleSlotNotifications ::
    ( HasAwaitSlot s
    , Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member WalletEffect effs
    )
    => RequestHandler effs (Handlers s) (Event s)
handleSlotNotifications =
    maybeToHandler AwaitSlot.request
    >>> RequestHandler.handleSlotNotifications
    >>^ AwaitSlot.event

handleBlockchainQueries ::
    ( HasWriteTx s
    , HasUtxoAt s
    , HasTxConfirmation s
    , HasOwnPubKey s
    , HasWatchAddress s
    , HasOwnId s
    , HasContractNotify s
    , HasAwaitSlot s
    )
    => RequestHandler (Reader ContractInstanceId ': ContractRuntimeEffect ': EmulatedWalletEffects) (Handlers s) (Event s)
handleBlockchainQueries =
    handlePendingTransactions
    <> handleUtxoQueries
    <> handleTxConfirmedQueries
    <> handleOwnPubKeyQueries
    <> handleNextTxAtQueries
    <> handleOwnInstanceIdQueries
    <> handleContractNotifications
    <> handleSlotNotifications

-- | Submit the wallet's pending transactions to the blockchain
--   and inform all wallets about new transactions and respond to
--   UTXO queries
handlePendingTransactions ::
    ( HasWriteTx s
    , Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member WalletEffect effs
    , Member SigningProcessEffect effs
    , Member ChainIndexEffect effs
    , Member (LogMsg TxBalanceMsg) effs
    )
    => RequestHandler effs (Handlers s) (Event s)
handlePendingTransactions =
    maybeToHandler WriteTx.pendingTransaction
    >>> RequestHandler.handlePendingTransactions
    >>^ WriteTx.event . view (from WriteTx.writeTxResponse)

-- | Look at the "utxo-at" requests of the contract and respond to all of them
--   with the current UTXO set at the given address.
handleUtxoQueries ::
    ( HasUtxoAt s
    , Member (LogObserve (LogMessage Text)) effs
    , Member ChainIndexEffect effs
    )
    => RequestHandler effs (Handlers s) (Event s)
handleUtxoQueries =
    maybeToHandler UtxoAt.utxoAtRequest
    >>> RequestHandler.handleUtxoQueries
    >>^ UtxoAt.event

handleTxConfirmedQueries ::
    ( HasTxConfirmation s
    , Member (LogObserve (LogMessage Text)) effs
    , Member ChainIndexEffect effs
    )
    => RequestHandler effs (Handlers s) (Event s)
handleTxConfirmedQueries =
    maybeToHandler AwaitTxConfirmed.txId
    >>> RequestHandler.handleTxConfirmedQueries
    >>^ AwaitTxConfirmed.event . unTxConfirmed

handleNextTxAtQueries ::
    ( HasWatchAddress s
    , Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member WalletEffect effs
    , Member ChainIndexEffect effs
    )
    => RequestHandler effs (Handlers s) (Event s)
handleNextTxAtQueries =
    maybeToHandler WatchAddress.watchAddressRequest
    >>> RequestHandler.handleNextTxAtQueries
    >>^ WatchAddress.event

handleOwnPubKeyQueries ::
    ( HasOwnPubKey s
    , Member (LogObserve (LogMessage Text)) effs
    , Member WalletEffect effs
    )
    => RequestHandler effs (Handlers s) (Event s)
handleOwnPubKeyQueries =
    maybeToHandler OwnPubKey.request
    >>> RequestHandler.handleOwnPubKey
    >>^ OwnPubKey.event

handleOwnInstanceIdQueries ::
    ( HasOwnId s
    , Member (LogObserve (LogMessage Text)) effs
    , Member (Reader ContractInstanceId) effs
    )
    => RequestHandler effs (Handlers s) (Event s)
handleOwnInstanceIdQueries =
    maybeToHandler OwnInstance.request
    >>> RequestHandler.handleOwnInstanceIdQueries
    >>^ OwnInstance.event

handleContractNotifications ::
    ( HasContractNotify s
    , Member (LogObserve (LogMessage Text)) effs
    , Member ContractRuntimeEffect effs
    )
    => RequestHandler effs (Handlers s) (Event s)
handleContractNotifications =
    maybeToHandler Notify.request
    >>> RequestHandler.handleContractNotifications
    >>^ Notify.event

-- | The wallets used in mockchain simulations by default. There are
--   ten wallets because the emulator comes with ten private keys.
allWallets :: [EM.Wallet]
allWallets = EM.Wallet <$> [1 .. 10]

defaultDist :: InitialDistribution
defaultDist = defaultDistFor allWallets

defaultDistFor :: [EM.Wallet] -> InitialDistribution
defaultDistFor wallets = Map.fromList $ zip wallets (repeat (Ada.lovelaceValueOf 10000))

makeClassyPrisms ''TraceError

instance EM.AsAssertionError (TraceError e) where
    _AssertionError = _TraceAssertionError
