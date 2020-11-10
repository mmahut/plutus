{-# LANGUAGE OverloadedStrings   #-}
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
    , EmulatorTrace
    , EmulatorErr(..)
    , ContractHandle(..)
    , ContractInstanceTag(..)
    , ContractConstraints
    -- * Constructing Traces
    , Types.activateContract
    , Types.activateContractWallet
    , Types.walletInstanceTag
    , Types.callEndpoint
    , Types.setSigningProcess
    , Types.payToWallet
    , Types.waitUntilSlot
    , Types.waitNSlots
    -- ** Inspecting the chain state
    , Types.chainState
    , ChainState.chainNewestFirst
    , ChainState.txPool
    , ChainState.index
    , ChainState.currentSlot
    -- ** Inspecting the agent states
    , Types.agentState
    , Wallet.ownPrivateKey
    , Wallet.nodeClient
    , Wallet.chainIndex
    , Wallet.signingProcess
    -- * Running traces
    , EmulatorConfig(..)
    , initialDistribution
    , defaultEmulatorConfig
    , runEmulatorStream
    -- * Interpreter
    , interpretEmulatorTrace
    , emInterpreter
    , emRunGlobal
    ) where

import           Control.Lens
import           Control.Monad                                   (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine                   (Yield)
import           Control.Monad.Freer.Error                       (Error)
import           Control.Monad.Freer.Extras                      (raiseEnd)
import           Control.Monad.Freer.Log                         (LogMessage (..), LogMsg (..),
                                                                  mapLog)
import           Control.Monad.Freer.Reader                      (runReader)
import           Control.Monad.Freer.State                       (State, evalState, gets)
import qualified Data.Aeson                                      as JSON
import qualified Data.Map                                        as Map
import           Data.Maybe                                      (fromMaybe)
import           Data.Proxy                                      (Proxy)
import           Language.Plutus.Contract                        (Contract, HasEndpoint, HasBlockchainActions)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoint
import Ledger.Interval (Interval)
import Ledger.TxId (TxId)
import Ledger.Tx (txId)
import           Ledger.Slot                                     (Slot (..))
import           Ledger.Value                                    (Value)
import           Plutus.Trace.Scheduler                          (Priority (..), SysCall (..), SystemCall,
                                                                  Tag, fork, mkSysCall, runThreads, sleep)
import           Wallet.API                                      (payToPublicKey)
import qualified Wallet.Emulator                                 as EM
import           Wallet.Emulator.Chain                           (ChainControlEffect, ChainEffect)
import qualified Wallet.Emulator.Chain                           as ChainState
import           Wallet.Emulator.MultiAgent                      (EmulatorEvent, EmulatorEvent' (..), EmulatorState,
                                                                  MultiAgentEffect,
                                                                  schedulerEvent, walletAction,
                                                                  walletControlAction)
import           Wallet.Emulator.Wallet                          (SigningProcess, Wallet (..))
import qualified Wallet.Emulator.Wallet                          as Wallet
import qualified Wallet.Emulator.Wallet                          as W
import Wallet.Emulator.Stream (runTraceStream, EmulatorConfig(..), EmulatorErr(..), initialDistribution, defaultEmulatorConfig)

import           Plutus.Trace.Effects.ContractInstanceId         (ContractInstanceIdEff, handleDeterministicIds, nextId)
import           Plutus.Trace.Emulator.ContractInstance          (ContractInstanceError, contractThread, getThread)
import           Plutus.Trace.Emulator.System                    (launchSystemThreads)
import           Plutus.Trace.Emulator.Types                     (ContractConstraints, ContractHandle (..), Emulator,
                                                                  EmulatorGlobal (..), EmulatorLocal (..),
                                                                  EmulatorMessage (..), EmulatorThreads, ContractInstanceTag)
import qualified Plutus.Trace.Emulator.Types                     as Types
import           Plutus.Trace.Types
import Streaming (Stream)
import Streaming.Prelude (Of)

type EmulatorTrace a = Eff '[Trace Emulator] a

-- | Run a 'Trace Emulator', streaming the log messages as they arrive
runEmulatorStream :: forall effs a.
    EmulatorConfig
    -> Eff '[Trace Emulator] a
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr)
runEmulatorStream conf = runTraceStream conf . interpretEmulatorTrace (conf ^. initialDistribution . to Map.keys)

-- | Interpret a 'Trace Emulator' action in the multi agent and emulated
--   blockchain effects.
interpretEmulatorTrace :: forall effs a.
    ( Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member ChainEffect effs
    , Member ChainControlEffect effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (State EmulatorState) effs
    )
    => [Wallet] -- ^ Wallets that should be simulated in the emulator
    -> Eff '[Trace Emulator] a
    -> Eff effs ()
interpretEmulatorTrace wallets action =
    evalState @EmulatorThreads mempty
        $ handleDeterministicIds
        $ interpret (mapLog (review schedulerEvent))
        $ runThreads
        $ do
            launchSystemThreads wallets
            interpret (handleTrace emInterpreter) $ void $ raiseEnd action

emInterpreter :: forall effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (State EmulatorState) effs
    )
    => TraceInterpreter Emulator effs EmulatorMessage
emInterpreter = TraceInterpreter
    { _runLocal = emRunLocal
    , _runGlobal = emRunGlobal
    }

emRunLocal :: forall b effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (State EmulatorState) effs
    )
    => Wallet
    -> EmulatorLocal b
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) b
emRunLocal wllt = \case
    ActivateContract tag con -> activate wllt con tag
    CallEndpointEm p h v -> callEndpoint p h v
    PayToWallet range target vl -> payToWallet range wllt target vl
    SetSigningProcess sp -> setSigningProcess wllt sp
    AgentState -> gets @EmulatorState (fromMaybe (Wallet.emptyWalletState wllt) . view (EM.walletStates . at wllt))

payToWallet :: forall effs.
    ( Member MultiAgentEffect effs )
    => Interval Slot
    -> Wallet
    -> Wallet
    -> Value
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) TxId
payToWallet range source target amount = do
    -- TODO: Maybe this should also happen in a separate thread?
    walletAction source $ fmap txId $ payToPublicKey range amount (EM.walletPubKey target)

setSigningProcess :: forall effs.
    ( Member MultiAgentEffect effs )
    => Wallet
    -> SigningProcess
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) ()
setSigningProcess wllt sp = void $ fork @effs @EmulatorMessage setSigningProcessTag High st
    where st = walletControlAction wllt $ W.setSigningProcess sp

setSigningProcessTag :: Tag
setSigningProcessTag = "set signing process"

activate :: forall s e effs.
    ( ContractConstraints s
    , Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member (LogMsg EmulatorEvent') effs
    , HasBlockchainActions s
    )
    => Wallet
    -> ContractInstanceTag
    -> Contract s e ()
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) (ContractHandle s e)
activate wllt tag con = do
    i <- nextId
    let handle = ContractHandle{chContract=con, chInstanceId = i, chInstanceTag = tag}
    _ <- fork @effs @EmulatorMessage runningContractInstanceTag High (runReader wllt $ interpret (mapLog InstanceEvent) $ reinterpret (mapLog InstanceEvent) $ contractThread handle)
    pure handle

runningContractInstanceTag :: Tag
runningContractInstanceTag = "contract instance"

callEndpoint :: forall s l e ep effs.
    ( ContractConstraints s
    , HasEndpoint l ep s
    , Member (State EmulatorThreads) effs
    , Member (Error ContractInstanceError) effs
    )
    => Proxy l
    -> ContractHandle s e
    -> ep
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) ()
callEndpoint _ ContractHandle{chInstanceId} ep = do
    let epJson = JSON.toJSON $ Endpoint.event @l @ep @s ep
        thr = do
            threadId <- getThread chInstanceId
            void $ mkSysCall @effs @EmulatorMessage High (Message threadId $ EndpointCall epJson)
    void $ fork @effs @EmulatorMessage callEndpointTag Low thr

callEndpointTag :: Tag
callEndpointTag = "call endpoint"

emRunGlobal :: forall b effs.
    Member (State EmulatorState) effs
    => EmulatorGlobal b
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) b
emRunGlobal = \case
    WaitUntilSlot s -> go where
        go = sleep @effs Sleeping >>= \case { Just (NewSlot sl) | sl >= s -> pure sl; _ -> go }
    ChainState  -> gets (view EM.chainState)
