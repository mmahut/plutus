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

module Plutus.Trace.Emulator.ContractInstance(
    contractThread
    ) where

import           Control.Lens
import           Control.Monad                                   (guard, void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader                      (Reader, ask, runReader)
import           Control.Monad.Freer.State                       (State, evalState, gets, modify)
import qualified Data.Aeson.Parser                               as JSON
import qualified Data.Aeson.Types                                as JSON
import           Data.Foldable                                   (traverse_)
import qualified Data.Row.Extras                                 as V
import qualified Data.Row.Internal                               as V
import qualified Data.Row.Variants                               as V
import           Data.Sequence                                   (Seq)
import           Language.Plutus.Contract                        (Contract (..), HasAwaitSlot, HasTxConfirmation,
                                                                  HasUtxoAt, HasWatchAddress, HasWriteTx, mapError)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoint
import           Language.Plutus.Contract.Resumable              (Request (..), Requests (..), Response (..))
import qualified Language.Plutus.Contract.Resumable              as State
import           Language.Plutus.Contract.Schema                 (Event (..), Handlers (..), Input, Output)
import           Language.Plutus.Contract.Trace.RequestHandler   (RequestHandler (..), tryHandler, wrapHandler)
import           Language.Plutus.Contract.Types                  (ResumableResult (..))
import qualified Language.Plutus.Contract.Types                  as Contract.Types
import           Plutus.Trace.Emulator.Types                     (ContractHandle (..), EmulatorAgentThreadEffs,
                                                                  EmulatorEvent (..), EmulatorState, instanceIdThreads)
import           Plutus.Trace.Scheduler                          (Priority (..), SuspendedThread, SysCall (..),
                                                                  SystemCall (..), ThreadId, mkSysCall, mkThread)
import           Wallet.Emulator.MultiAgent                      (EmulatedWalletEffects, MultiAgentEffect, walletAction)
import           Wallet.Emulator.Wallet                          (Wallet)
import           Wallet.Types                                    (ContractInstanceId)

-- | Effects available to threads that run in the context of specific
--   agents (ie wallets)
type ContractInstanceThreadEffs s e a effs =
    State (ContractInstanceState s e a)
    ': EmulatorAgentThreadEffs effs

contractThread :: forall s e effs.
    ( Member (State EmulatorState) effs
    , Member MultiAgentEffect effs
    , V.Forall (Output s) V.Unconstrained1
    , V.AllUniqueLabels (Input s)
    , V.Forall (Input s) JSON.FromJSON
    )
    => ContractHandle s e
    -> Eff (EmulatorAgentThreadEffs effs) ()
contractThread ContractHandle{chInstanceId, chContract} = do
    ask @ThreadId >>= registerInstance chInstanceId
    evalState (emptyInstanceState chContract) $ do
        msg <- mkSysCall @effs @EmulatorEvent Low Suspend
        runInstance msg

registerInstance :: forall effs.
    ( Member (State EmulatorState) effs )
    => ContractInstanceId
    -> ThreadId
    -> Eff effs ()
registerInstance i t = modify (instanceIdThreads . at i .~ Just t)

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
    , V.Forall (Output s) V.Unconstrained1
    , V.AllUniqueLabels (Input s)
    , V.Forall (Input s) JSON.FromJSON
    )
    => Maybe EmulatorEvent
    -> Eff (ContractInstanceThreadEffs s e a effs) ()
runInstance event = do
    case event of
        Just (EndpointCall endpointName vl) -> do
            -- check if the endpoint is active and throw an error if it isn't
            hks <- getHooks @s @e @a
            -- event <- decodeJSON @(Event s) -- ??
            -- TODO: What to do if endpoint is not active? -> Configurable (wait or error)
            void $ respondToRequest @s @e @a $ RequestHandler $ \(Handlers v) -> do
                guard $ (V.eraseWithLabels @V.Unconstrained1 (const ()) v) == (endpointName, ())
                let result = JSON.fromJSON @(Event s) vl
                case result of
                    JSON.Error e       -> error e -- FIXME
                    JSON.Success event -> pure event
            pure ()
        _ -> do
            -- see if we can handle any requests
            mkSysCall @effs @EmulatorEvent Low Suspend >>= runInstance

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
