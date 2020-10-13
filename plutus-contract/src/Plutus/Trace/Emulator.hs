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
    , interpretSimulatorEm
    , ContractHandle(..)
    -- * Constructing Traces
    , Types.activateContract
    , Types.callEndpoint
    , Types.payToWallet
    , Types.waitUntilSlot
    -- * Interpreter
    , emInterpreter
    ) where

import           Control.Monad                                   (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine                   (Yield)
import           Control.Monad.Freer.Error                       (Error)
import           Control.Monad.Freer.Extras                      (raiseEnd)
import           Control.Monad.Freer.Reader                      (runReader)
import           Control.Monad.Freer.State                       (State, evalState)
import qualified Data.Aeson                                      as JSON
import           Data.Proxy                                      (Proxy)
import           Language.Plutus.Contract                        (Contract, HasEndpoint)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoint
import           Ledger.Value                                    (Value)
import           Plutus.Trace.Scheduler                          (Priority (..), SysCall (..), SystemCall, fork,
                                                                  mkSysCall, runThreads, sleep)
import           Wallet.API                                      (defaultSlotRange, payToPublicKey_)
import qualified Wallet.Emulator                                 as EM
import           Wallet.Emulator.Chain                           (ChainControlEffect, ChainEffect)
import           Wallet.Emulator.MultiAgent                      (MultiAgentEffect, walletAction)
import           Wallet.Emulator.Wallet                          (Wallet (..))

import           Plutus.Trace.Effects.ContractInstanceId         (ContractInstanceIdEff, handleDeterministicIds, nextId)
import           Plutus.Trace.Emulator.ContractInstance          (ContractInstanceError, contractThread, getThread)
import           Plutus.Trace.Emulator.System                    (launchSystemThreads)
import           Plutus.Trace.Emulator.Types                     (ContractConstraints, ContractHandle (..), Emulator,
                                                                  EmulatorEvent (..), EmulatorGlobal (..),
                                                                  EmulatorLocal (..), EmulatorThreads)
import qualified Plutus.Trace.Emulator.Types                     as Types
import           Plutus.Trace.Types


-- runTraceWithInitialStates ::
--     forall s e a b.
--     ( V.AllUniqueLabels (Input s)
--     , V.Forall (Input s) JSON.FromJSON
--     , V.Forall (Output s) V.Unconstrained1
--     )
--     => EmulatorState
--     -> ContractTraceState s (TraceError e) a
--     -> Eff (ContractTraceEffs s e a) b
--     -> (Either (TraceError e) (b, ContractTraceState s (TraceError e) a), EmulatorState)
-- runTraceWithInitialStates initialEmulatorState initialContractState action =
--     EM.runEmulator initialEmulatorState
--         $ runState initialContractState
--         $ interpret (Eff.writeIntoState EM.emulatorLog)
--         $ reinterpret @_ @(Writer [LogMessage EM.EmulatorEvent]) (handleLogWriter _singleton)
--         $ reinterpret @_ @(LogMsg EM.EmulatorEvent) (mapMLog makeTimed)
--         $ reinterpret @_ @(LogMsg EmulatorNotifyLogMsg) (handleEmulatorContractNotify @s @e @a)
--         $ action

-- makeTimed :: Member (State EmulatorState) effs => EmulatorNotifyLogMsg -> Eff effs EM.EmulatorEvent
-- makeTimed e = do
--     emulatorTime <- gets (view (EM.chainState . EM.currentSlot))
--     pure $ review (EM.emulatorTimeEvent emulatorTime) (EM.NotificationEvent e)

-- -- | Run a trace in the emulator and return the final state alongside the
-- --   result
-- runTraceWithDistribution ::
--     forall s e a b.
--     ( V.AllUniqueLabels (Input s)
--     , V.Forall (Input s) JSON.FromJSON
--     , V.Forall (Output s) V.Unconstrained1
--     )
--     => InitialDistribution
--     -> Contract s e a
--     -> Eff (ContractTraceEffs s e a) b
--     -> (Either (TraceError e) (b, ContractTraceState s (TraceError e) a), EmulatorState)
-- runTraceWithDistribution dist con action =
--     let -- make sure the wallets know about the initial transaction
--         notifyInitial = void (EM.addBlocksAndNotify (Map.keys dist) 1)
--         action' = EM.processEmulated @(TraceError e) notifyInitial >> action
--         con' = mapError TContractError con
--         s = EM.emulatorStateInitialDist (Map.mapKeys EM.walletPubKey dist)
--         c = initState (Map.keys dist) con'
--     in runTraceWithInitialStates s c action'


-- | Interpret a 'Simulator Emulator' action in the multi agent and emulated
--   blockchain effects.
interpretSimulatorEm :: forall effs.
    ( Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member ChainEffect effs
    , Member ChainControlEffect effs
    )
    => Eff '[Simulator Emulator] ()
    -> Eff effs ()
interpretSimulatorEm action =
    evalState @EmulatorThreads mempty
        $ handleDeterministicIds
        $ runThreads
        $ do
            launchSystemThreads
            interpret (handleSimulator emInterpreter) $ raiseEnd action
emInterpreter :: forall effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    )
    => SimulatorInterpreter Emulator effs EmulatorEvent
emInterpreter = SimulatorInterpreter
    { _runLocal = emRunLocal
    , _runGlobal = emRunGlobal
    }

emRunLocal :: forall b effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    )
    => Wallet
    -> EmulatorLocal b
    -> Eff (Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent) ': effs) b
emRunLocal wllt = \case
    ActivateContract con -> activate wllt con
    CallEndpointEm p h v -> callEndpoint p h v
    PayToWallet target vl -> payToWallet wllt target vl

payToWallet :: forall effs.
    ( Member MultiAgentEffect effs )
    => Wallet
    -> Wallet
    -> Value
    -> Eff (Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent) ': effs) ()
payToWallet source target amount = void $ fork @effs @EmulatorEvent High payment
    where payment = walletAction source $ payToPublicKey_ defaultSlotRange amount (EM.walletPubKey target)

activate :: forall s e effs.
    ( ContractConstraints s
    , Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    )
    => Wallet
    -> Contract s e ()
    -> Eff (Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent) ': effs) (ContractHandle s e)
activate wllt con = do
    i <- nextId
    let handle = ContractHandle{chContract=con, chInstanceId = i}
    _ <- fork @effs @EmulatorEvent High (runReader wllt $ contractThread handle)
    pure handle

callEndpoint :: forall s l e ep effs.
    ( ContractConstraints s
    , HasEndpoint l ep s
    , Member (State EmulatorThreads) effs
    , Member (Error ContractInstanceError) effs
    )
    => Proxy l
    -> ContractHandle s e
    -> ep
    -> Eff (Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent) ': effs) ()
callEndpoint _ ContractHandle{chInstanceId} ep = do
    threadId <- getThread chInstanceId
    let epJson = JSON.toJSON $ Endpoint.event @l @ep @s ep
        thr = void $ mkSysCall @effs @EmulatorEvent High (Message threadId $ EndpointCall epJson)
    void $ fork @effs @EmulatorEvent High thr

emRunGlobal :: forall b effs.
    EmulatorGlobal b
    -> Eff (Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent) ': effs) b
emRunGlobal = \case
    WaitUntilSlot s -> go where
        go = sleep @effs Sleeping >>= \case { Just (NewSlot sl) | sl >= s -> pure sl; _ -> go }
