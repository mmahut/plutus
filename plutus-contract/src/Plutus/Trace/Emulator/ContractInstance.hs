{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Emulator.ContractInstance(
    contractThread
    , getThread
    , ContractInstanceError
    -- * Instance state
    , ContractInstanceState(..)
    , emptyInstanceState
    , addEventInstanceState
    ) where

import           Control.Lens
import           Control.Monad                                 (guard, unless, void, when)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine                 (Yield)
import           Control.Monad.Freer.Error                     (Error, throwError)
import           Control.Monad.Freer.Extras                    (raiseEnd11)
import           Control.Monad.Freer.Log                       (LogMessage, LogMsg(..), LogObserve, logDebug, logError,
                                                                logInfo, mapLog)
import           Control.Monad.Freer.Reader                    (Reader, ask, runReader)
import           Control.Monad.Freer.State                     (State, evalState, gets, modify, get, put)
import           Data.Aeson                                    (FromJSON, ToJSON)
import qualified Data.Aeson.Types                              as JSON
import           Data.Foldable                                 (traverse_)
import           Data.Sequence                                 (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text                                     as T
import           GHC.Generics                                  (Generic)
import           Language.Plutus.Contract                      (Contract (..), HasBlockchainActions)
import           Language.Plutus.Contract.Resumable            (Request (..), Requests (..), Response (..))
import qualified Language.Plutus.Contract.Resumable            as State
import           Language.Plutus.Contract.Schema               (Event (..), Handlers (..), eventName, handlerName)
import           Language.Plutus.Contract.Trace                (handleBlockchainQueries)
import           Language.Plutus.Contract.Trace.RequestHandler (RequestHandler (..), RequestHandlerLogMsg, tryHandler,
                                                                wrapHandler)
import           Language.Plutus.Contract.Types                (ResumableResult (..))
import qualified Language.Plutus.Contract.Types                as Contract.Types
import           Plutus.Trace.Emulator.Types                   (ContractConstraints, ContractHandle (..),
                                                                ContractInstanceError (..), ContractInstanceLog (..),
                                                                ContractInstanceMsg (..), EmulatorAgentThreadEffs,
                                                                EmulatorMessage (..), EmulatorThreads,
                                                                instanceIdThreads)
import           Plutus.Trace.Scheduler                        (Priority (..), SysCall (..), SystemCall, ThreadId,
                                                                mkSysCall, sleep)
import qualified Wallet.API                                    as WAPI
import           Wallet.Effects                                (ChainIndexEffect, ContractRuntimeEffect (..),
                                                                NodeClientEffect, SigningProcessEffect, WalletEffect)
import           Wallet.Emulator.LogMessages                   (TxBalanceMsg)
import           Wallet.Emulator.MultiAgent                    (EmulatedWalletEffects, MultiAgentEffect, walletAction)
import           Wallet.Emulator.Wallet                        (Wallet)
import           Wallet.Types                                  (ContractInstanceId, Notification (..),
                                                                NotificationError (..))

-- | Effects available to threads that run in the context of specific
--   agents (ie wallets)
type ContractInstanceThreadEffs s e a effs =
    LogMsg ContractInstanceMsg
    ': State (ContractInstanceState s e a)
    ': Reader ContractInstanceId
    ': ContractRuntimeEffect
    ': EmulatorAgentThreadEffs effs

handleContractRuntime ::
    forall effs effs2.
    ( Member (State EmulatorThreads) effs2
    , Member (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage)) effs2
    )
    => Eff (ContractRuntimeEffect ': effs2)
    ~> Eff effs2
handleContractRuntime = interpret $ \case
    SendNotification n@Notification{notificationContractID} -> do
        target <- gets (view $ instanceIdThreads . at notificationContractID)
        case target of
            Nothing -> pure $ Just $ InstanceDoesNotExist notificationContractID
            Just threadId -> do
                let e = Message threadId (Notify n)
                _ <- mkSysCall @effs @EmulatorMessage High e
                pure Nothing

contractThread :: forall s e effs.
    ( Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , ContractConstraints s
    , HasBlockchainActions s
    )
    => ContractHandle s e
    -> Eff (EmulatorAgentThreadEffs effs) ()
contractThread ContractHandle{chInstanceId, chContract, chInstanceTag} = do
    ask @ThreadId >>= registerInstance chInstanceId
    handleContractRuntime @effs
        $ runReader chInstanceId
        $ evalState (emptyInstanceState chContract)
        $ interpret (mapLog (\m -> ContractInstanceLog m chInstanceId chInstanceTag))
        $ do
            logInfo Started
            logNewMessages @s @e @() Seq.empty
            logCurrentRequests
            msg <- mkSysCall @effs @EmulatorMessage Low Suspend
            runInstance msg

registerInstance :: forall effs.
    ( Member (State EmulatorThreads) effs )
    => ContractInstanceId
    -> ThreadId
    -> Eff effs ()
registerInstance i t = modify (instanceIdThreads . at i .~ Just t)

getThread :: forall effs.
    ( Member (State EmulatorThreads) effs
    , Member (Error ContractInstanceError) effs
    )
    => ContractInstanceId
    -> Eff effs ThreadId
getThread t = do
    r <- gets (view $ instanceIdThreads . at t)
    maybe (throwError $ ThreadIdNotFound t) pure r

data ContractInstanceState s e a =
    ContractInstanceState
        { instContractState   :: ResumableResult e (Event s) (Handlers s) a
        , instEvents          :: Seq (Response (Event s))
        , instHandlersHistory :: Seq [State.Request (Handlers s)]
        , instContract        :: Contract s e a
        }

emptyInstanceState :: Contract s e a -> ContractInstanceState s e a
emptyInstanceState con@(Contract c) =
    ContractInstanceState
        { instContractState = Contract.Types.runResumable [] mempty c
        , instEvents = mempty
        , instHandlersHistory = mempty
        , instContract = con
        }

-- | Run an instance of a contract
runInstance :: forall s e a effs.
    ( Member MultiAgentEffect effs
    , ContractConstraints s
    , HasBlockchainActions s
    , Member (Error ContractInstanceError) effs
    )
    => Maybe EmulatorMessage
    -> Eff (ContractInstanceThreadEffs s e a effs) ()
runInstance event = do
    hks <- getHooks @s @e @a
    when (null hks) $ logInfo Stopped
    unless (null hks) $ do
        case event of
            Just (EndpointCall vl) -> do
                logInfo $ ReceiveEndpointCall vl
                -- TODO:
                -- check if the endpoint is active and (maybe - configurable) throw an error if it isn't
                e <- case JSON.fromJSON @(Event s) vl of
                        JSON.Error e'       -> do
                            let msg = JSONDecodingError e'
                            logError $ InstErr msg
                            throwError msg
                        JSON.Success event' -> pure event'

                response <- respondToRequest @s @e @a $ RequestHandler $ \h -> do
                    guard $ handlerName h == eventName e
                    pure e
                logResponse response
                sleep @effs Low >>= runInstance
            _ -> do
                -- FIXME: handleSlotNotifications configurable
                response <- respondToRequest @s @e @a handleBlockchainQueries
                let prio =
                        maybe
                            -- If no events could be handled we go to sleep
                            -- with the lowest priority, waking only after
                            -- some external event has happened, for example
                            -- when a new block was added.
                            Sleeping

                            -- If an event was handled we go to sleep with
                            -- a low priority, trying again after all other
                            -- active threads have had their turn
                            (const Low)
                            response
                logResponse response
                sleep @effs prio >>= runInstance

getHooks :: forall s e a effs. Member (State (ContractInstanceState s e a)) effs => Eff effs [Request (Handlers s)]
getHooks = State.unRequests . wcsRequests <$> gets @(ContractInstanceState s e a) instContractState

-- | Add a 'Response' to the contract instance state
addResponse
    :: forall s e a effs.
    ( Member (State (ContractInstanceState s e a)) effs
    , Member (LogMsg ContractInstanceMsg) effs
    )
    => Response (Event s)
    -> Eff effs ()
addResponse e = do
    oldState <- get @(ContractInstanceState s e a)
    let newState = addEventInstanceState e oldState
    put newState
    logNewMessages @s @e @a (wcsLogs $ instContractState oldState)

raiseWallet :: forall f effs.
    ( Member f EmulatedWalletEffects
    , Member MultiAgentEffect effs
    )
    => Wallet
    -> f
    ~> Eff effs
raiseWallet wllt = walletAction wllt . send

type ContractInstanceRequests effs =
        Reader ContractInstanceId
         ': ContractRuntimeEffect
         ': WalletEffect
         ': Error WAPI.WalletAPIError
         ': NodeClientEffect
         ': ChainIndexEffect
         ': SigningProcessEffect
         ': LogObserve (LogMessage T.Text)
         ': LogMsg RequestHandlerLogMsg
         ': LogMsg TxBalanceMsg
         ': LogMsg T.Text
         ': effs

-- | Inspect the open requests of a contract instance,
--   and maybe respond to them. Returns the response that was provided to the
--   contract, if any.
respondToRequest :: forall s e a effs.
    ( Member (State (ContractInstanceState s e a)) effs
    , Member MultiAgentEffect effs
    , Member (Reader Wallet) effs
    , Member ContractRuntimeEffect effs
    , Member (Reader ContractInstanceId) effs
    , Member (LogMsg ContractInstanceMsg) effs
    )
    => RequestHandler (Reader ContractInstanceId ': ContractRuntimeEffect ': EmulatedWalletEffects) (Handlers s) (Event s)
    -- ^ How to respond to the requests.
    ->  Eff effs (Maybe (Response (Event s)))
respondToRequest f = do
    hks <- getHooks @s @e @a
    ownWallet <- ask @Wallet
    let hdl :: (Eff (Reader ContractInstanceId ': ContractRuntimeEffect ': EmulatedWalletEffects) (Maybe (Response (Event s)))) = tryHandler (wrapHandler f) hks
        hdl' :: (Eff (ContractInstanceRequests effs) (Maybe (Response (Event s)))) = raiseEnd11 hdl

        response_ :: Eff effs (Maybe (Response (Event s))) =
            interpret (raiseWallet @(LogMsg T.Text) ownWallet)
                $ interpret (raiseWallet @(LogMsg TxBalanceMsg) ownWallet)
                $ interpret (raiseWallet @(LogMsg RequestHandlerLogMsg) ownWallet)
                $ interpret (raiseWallet @(LogObserve (LogMessage T.Text)) ownWallet)
                $ interpret (raiseWallet @SigningProcessEffect ownWallet)
                $ interpret (raiseWallet @ChainIndexEffect ownWallet)
                $ interpret (raiseWallet @NodeClientEffect ownWallet)
                $ interpret (raiseWallet @(Error WAPI.WalletAPIError) ownWallet)
                $ interpret (raiseWallet @WalletEffect ownWallet)
                $ subsume @ContractRuntimeEffect
                $ subsume @(Reader ContractInstanceId) hdl'
    response <- response_
    traverse_ (addResponse @s @e @a) response
    pure response

addEventInstanceState :: forall s e a.
    Response (Event s)
    -> ContractInstanceState s e a
    -> ContractInstanceState s e a
addEventInstanceState event s@ContractInstanceState{instContractState, instEvents, instHandlersHistory, instContract=Contract c} =
    let ResumableResult{wcsResponses,wcsRequests=Requests{unRequests},wcsCheckpointStore} = instContractState
        state' = Contract.Types.insertAndUpdate c wcsCheckpointStore wcsResponses event
        events' = instEvents |> event
        history' = instHandlersHistory |> unRequests
    in s { instContractState = state', instEvents = events', instHandlersHistory = history'}

---
-- Logging
---

logResponse ::  forall s e a effs.
    ( Member MultiAgentEffect effs
    , ContractConstraints s
    , HasBlockchainActions s
    , Member (Error ContractInstanceError) effs
    )
    => Maybe (Response (Event s))
    -> Eff (ContractInstanceThreadEffs s e a effs) ()
logResponse = \case
    Nothing -> logDebug NoRequestsHandled
    Just rsp -> do
        logInfo $ HandledRequest $ fmap JSON.toJSON rsp
        logCurrentRequests

logCurrentRequests :: forall s e a effs.
    ( Member MultiAgentEffect effs
    , ContractConstraints s
    , HasBlockchainActions s
    , Member (Error ContractInstanceError) effs
    )
    => Eff (ContractInstanceThreadEffs s e a effs) ()
logCurrentRequests = do
    hks <- getHooks @s @e @a
    logInfo $ CurrentRequests $ fmap (fmap JSON.toJSON) hks

logNewMessages :: forall s e a effs.
    ( Member (LogMsg ContractInstanceMsg) effs
    , Member (State (ContractInstanceState s e a)) effs
    )
    => Seq (LogMessage JSON.Value) -- old messages
    -> Eff effs ()
logNewMessages oldMessages = do
    newState <- get @(ContractInstanceState s e a)
    let contractLogs = wcsLogs $ instContractState newState
        newContractLogs = Seq.drop (Seq.length oldMessages) contractLogs
    traverse_ (send . LMessage . fmap ContractLog) newContractLogs
