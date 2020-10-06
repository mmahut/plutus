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

import Control.Lens
import Plutus.Trace.Emulator.Types (EmulatorAgentThreadEffs, EmulatorState, ContractHandle(..), EmulatorEvent(..), instanceIdThreads)
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader     (Reader, ask, runReader)
import           Control.Monad.Freer.State      (State, modify, evalState, gets)
import           Plutus.Trace.Scheduler         (Priority (..), SystemCall (..), SuspendedThread, SysCall (..),
                                                 ThreadId, mkSysCall, mkThread)
import           Wallet.Types                   (ContractInstanceId)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint   as Endpoint
import Data.Foldable (traverse_)
import Language.Plutus.Contract.Trace.RequestHandler (RequestHandler(..), tryHandler, wrapHandler)
import           Language.Plutus.Contract                          (Contract (..), HasAwaitSlot, HasTxConfirmation,
                                                                    HasUtxoAt, HasWatchAddress, HasWriteTx, mapError)
import qualified Language.Plutus.Contract.Types                    as Contract.Types
import           Language.Plutus.Contract.Types                    (ResumableResult (..))
import           Language.Plutus.Contract.Schema                   (Event (..), Handlers (..), Input, Output)
import Data.Sequence (Seq)
import           Language.Plutus.Contract.Resumable                (Request (..), Requests (..), Response (..))
import qualified Language.Plutus.Contract.Resumable                as State
import Wallet.Emulator.MultiAgent (EmulatedWalletEffects, MultiAgentEffect, walletAction)
import Wallet.Emulator.Wallet (Wallet)

-- | Effects available to threads that run in the context of specific
--   agents (ie wallets)
type ContractInstanceThreadEffs s e a effs =
    State (ContractInstanceState s e a)
    ': EmulatorAgentThreadEffs effs

contractThread :: forall s e effs.
    ( Member (State EmulatorState) effs
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
    Maybe EmulatorEvent
    -> Eff (ContractInstanceThreadEffs s e a effs) ()
runInstance event = do
    case event of
        Just (EndpointCall ep vl) -> do
            -- check if the endpoint is active and throw an error if it isn't
            hks <- getHooks @s @e @a
            -- TODO: What to do if endpoint is not active? -> Configurable (wait or error)
            -- void $ respondToRequest @s @e @a $ RequestHandler $ \req -> do
            --     guard (Endpoint.isActive @l req)
            --     pure $ Endpoint.event @l ep
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
    -- pure response
    undefined

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
