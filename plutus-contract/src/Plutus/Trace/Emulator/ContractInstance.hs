{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Emulator.ContractInstance(
    contractThread
    , getThread
    , ContractInstanceError
    ) where

import           Control.Lens
import           Control.Monad                                 (guard, unless, void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error                     (Error, throwError)
import           Control.Monad.Freer.Reader                    (Reader, ask)
import           Control.Monad.Freer.State                     (State, evalState, gets, modify)
import qualified Data.Aeson.Types                              as JSON
import           Data.Foldable                                 (traverse_)
import           Data.Sequence                                 (Seq)
import           Language.Plutus.Contract                      (Contract (..))
import           Language.Plutus.Contract.Resumable            (Request (..), Requests (..), Response (..))
import qualified Language.Plutus.Contract.Resumable            as State
import           Language.Plutus.Contract.Schema               (Event (..), Handlers (..), eventName, handlerName)
import           Language.Plutus.Contract.Trace                (handleBlockchainQueries, handleSlotNotifications)
import           Language.Plutus.Contract.Trace.RequestHandler (RequestHandler (..), tryHandler, wrapHandler)
import           Language.Plutus.Contract.Types                (ResumableResult (..))
import qualified Language.Plutus.Contract.Types                as Contract.Types
import           Plutus.Trace.Emulator.Types                   (ContractConstraints, ContractHandle (..),
                                                                EmulatorAgentThreadEffs, EmulatorEvent (..),
                                                                EmulatorThreads, instanceIdThreads)
import           Plutus.Trace.Scheduler                        (Priority (..), SysCall (..), ThreadId, mkSysCall, sleep)
import           Wallet.Emulator.MultiAgent                    (EmulatedWalletEffects, MultiAgentEffect, walletAction)
import           Wallet.Emulator.Wallet                        (Wallet)
import           Wallet.Types                                  (ContractInstanceId)

-- | Effects available to threads that run in the context of specific
--   agents (ie wallets)
type ContractInstanceThreadEffs s e a effs =
    State (ContractInstanceState s e a)
    ': EmulatorAgentThreadEffs effs

contractThread :: forall s e effs.
    ( Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , ContractConstraints s
    )
    => ContractHandle s e
    -> Eff (EmulatorAgentThreadEffs effs) ()
contractThread ContractHandle{chInstanceId, chContract} = do
    ask @ThreadId >>= registerInstance chInstanceId
    evalState (emptyInstanceState chContract) $ do
        msg <- mkSysCall @effs @EmulatorEvent Low Suspend
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

data ContractInstanceError =
    ThreadIdNotFound ContractInstanceId
    | JSONDecodingError String

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
    , Member (Error ContractInstanceError) effs
    )
    => Maybe EmulatorEvent
    -> Eff (ContractInstanceThreadEffs s e a effs) ()
runInstance event = do
    hks <- getHooks @s @e @a
    -- FIXME: Log if hks == null (ie. the contract instance is done)
    unless (null hks) $ do
        case event of
            Just (EndpointCall vl) -> do
                -- TODO:
                -- check if the endpoint is active and (maybe - configurable) throw an error if it isn't
                e <- case JSON.fromJSON @(Event s) vl of
                        JSON.Error e'       -> throwError $ JSONDecodingError e'
                        JSON.Success event' -> pure event'

                void $ respondToRequest @s @e @a $ RequestHandler $ \h -> do
                    guard $ handlerName h == eventName e
                    pure e
                sleep @effs Low >>= runInstance
            _ -> do
                -- FIXME: handleSlotNotifications configurable
                responses <- respondToRequest @s @e @a (handleBlockchainQueries <> handleSlotNotifications)
                let prio =
                        maybe
                            -- If no events could be handled we go to sleep
                            -- with the lowest priority, awaking only after
                            -- some external event has happened, for example
                            -- when a new block was added.
                            Sleeping

                            -- If an event was handled we go to sleep with
                            -- a low priority, trying again after all other
                            -- active threads have had their turn
                            (const Low)
                            responses
                sleep @effs prio >>= runInstance

getHooks :: forall s e a effs. Member (State (ContractInstanceState s e a)) effs => Eff effs [Request (Handlers s)]
getHooks = State.unRequests . wcsRequests <$> gets @(ContractInstanceState s e a) instContractState

-- | Add a 'Response' to the contract instance state
addResponse
    :: forall s e a effs.
    ( Member (State (ContractInstanceState s e a)) effs
    )
    => Response (Event s)
    -> Eff effs ()
addResponse e = modify @(ContractInstanceState s e a) $ addEventInstanceState e

-- | Inspect the open requests of a contract instance,
--   and maybe respond to them. Returns the response that was provided to the
--   contract, if any.
respondToRequest :: forall s e a effs.
    ( Member (State (ContractInstanceState s e a)) effs
    , Member MultiAgentEffect effs
    , Member (Reader Wallet) effs
    )
    => RequestHandler EmulatedWalletEffects (Handlers s) (Event s)
    -- ^ How to respond to the requests.
    ->  Eff effs (Maybe (Response (Event s)))
respondToRequest f = do
    hks <- getHooks @s @e @a
    ownWallet <- ask @Wallet
    (response :: Maybe (Response (Event s))) <- walletAction ownWallet $ tryHandler (wrapHandler f) hks
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
