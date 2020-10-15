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
    , EmulatorErr(..)
    , ContractHandle(..)
    -- * Constructing Traces
    , Types.activateContract
    , Types.callEndpoint
    , Types.payToWallet
    , Types.waitUntilSlot
    -- * Running traces
    , EmulatorConfig(..)
    , defaultEmulatorConfig
    , runEmulatorTrace
    -- * Interpreter
    , interpretEmulatorTrace
    , emInterpreter
    ) where

import           Control.Lens
import           Control.Monad                                   (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine                   (Yield)
import           Control.Monad.Freer.Error                       (Error, runError)
import           Control.Monad.Freer.Extras                      (raiseEnd, raiseEnd4, raiseEnd5, wrapError,
                                                                  writeIntoState)
import           Control.Monad.Freer.Log                         (LogMessage, LogMsg (..), handleLogWriter, mapLog,
                                                                  mapMLog)
import           Control.Monad.Freer.Reader                      (runReader)
import           Control.Monad.Freer.State                       (State, evalState, gets, runState)
import           Control.Monad.Freer.Writer                      (Writer, tell)
import qualified Data.Aeson                                      as JSON
import qualified Data.Map                                        as Map
import           Data.Proxy                                      (Proxy)
import           Language.Plutus.Contract                        (Contract, HasEndpoint)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoint
import           Ledger.Slot                                     (Slot)
import           Ledger.Value                                    (Value)
import           Plutus.Trace.Scheduler                          (Priority (..), SysCall (..), SystemCall,
                                                                  ThreadType (..), fork, mkSysCall, runThreads, sleep)
import           Wallet.API                                      (WalletAPIError, defaultSlotRange, payToPublicKey_)
import qualified Wallet.Emulator                                 as EM
import           Wallet.Emulator.Chain                           (ChainControlEffect, ChainEffect, getCurrentSlot)
import           Wallet.Emulator.MultiAgent                      (EmulatorEvent, EmulatorEvent', EmulatorState,
                                                                  EmulatorTimeEvent (..), MultiAgentEffect, emulatorLog,
                                                                  emulatorTimeEvent, schedulerEvent, walletAction,
                                                                  walletControlAction)
import           Wallet.Emulator.Wallet                          (SigningProcess, Wallet (..))
import qualified Wallet.Emulator.Wallet                          as W

import           Language.Plutus.Contract.Trace                  (InitialDistribution, defaultDist)
import           Plutus.Trace.Effects.ContractInstanceId         (ContractInstanceIdEff, handleDeterministicIds, nextId)
import           Plutus.Trace.Emulator.ContractInstance          (ContractInstanceError, contractThread, getThread)
import           Plutus.Trace.Emulator.System                    (launchSystemThreads)
import           Plutus.Trace.Emulator.Types                     (ContractConstraints, ContractHandle (..), Emulator,
                                                                  EmulatorGlobal (..), EmulatorLocal (..),
                                                                  EmulatorMessage (..), EmulatorThreads)
import qualified Plutus.Trace.Emulator.Types                     as Types
import           Plutus.Trace.Types

-- | Run a 'Trace Emulator', returning the final state and possibly an error
runEmulatorTrace :: EmulatorConfig -> Eff '[Trace Emulator] () -> (Either EmulatorErr (), EM.EmulatorState)
runEmulatorTrace conf = runTraceBackend conf . interpretEmulatorTrace

data EmulatorErr =
    WalletErr WalletAPIError
    | AssertionErr EM.AssertionError
    | InstanceErr ContractInstanceError
    deriving (Show)

runTraceBackend ::
    EmulatorConfig
    -> Eff '[ LogMsg SchedulerLog
            , MultiAgentEffect
            , ChainEffect
            , ChainControlEffect
            , Error ContractInstanceError
            ] ()
    -> (Either EmulatorErr (), EM.EmulatorState)
runTraceBackend conf =
    run
    . runState (initialState conf)
    . interpret (writeIntoState emulatorLog)
    . interpret (handleLogWriter @EmulatorEvent @[LogMessage EmulatorEvent] (unto return))
    . reinterpret2 @_ @(LogMsg EmulatorEvent) @(Writer [LogMessage EmulatorEvent]) (mkTimedLogs @EmulatorEvent')
    . runError
    . wrapError WalletErr
    . wrapError AssertionErr
    . wrapError InstanceErr
    . EM.processEmulated
    . interpret (mapLog (review schedulerEvent))
    . raiseEnd5

-- | Annotate the emulator log messages with the current system time
--   (slot number)
mkTimedLogs :: forall a effs.
    ( Member (LogMsg (EmulatorTimeEvent a)) effs
    , Member (State EmulatorState) effs
    )
    => LogMsg a
    ~> Eff effs
mkTimedLogs = mapMLog f where
    f :: a -> Eff effs (EmulatorTimeEvent a)
    f a =
        EmulatorTimeEvent
            <$> gets (view $ EM.chainState . EM.currentSlot)
            <*> pure a

data EmulatorConfig =
    EmulatorConfig
        { emcInitialDistribution :: InitialDistribution
        }

defaultEmulatorConfig :: EmulatorConfig
defaultEmulatorConfig =
    EmulatorConfig
        { emcInitialDistribution = defaultDist
        }

initialState :: EmulatorConfig -> EM.EmulatorState
initialState EmulatorConfig{emcInitialDistribution} = EM.emulatorStateInitialDist (Map.mapKeys EM.walletPubKey emcInitialDistribution)

-- | Interpret a 'Trace Emulator' action in the multi agent and emulated
--   blockchain effects.
interpretEmulatorTrace :: forall effs.
    ( Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member ChainEffect effs
    , Member ChainControlEffect effs
    , Member (LogMsg SchedulerLog) effs
    )
    => Eff '[Trace Emulator] ()
    -> Eff effs ()
interpretEmulatorTrace action =
    evalState @EmulatorThreads mempty
        $ handleDeterministicIds
        $ runThreads
        $ do
            launchSystemThreads
            interpret (handleTrace emInterpreter) $ raiseEnd action

emInterpreter :: forall effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
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
    )
    => Wallet
    -> EmulatorLocal b
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) b
emRunLocal wllt = \case
    ActivateContract con -> activate wllt con
    CallEndpointEm p h v -> callEndpoint p h v
    PayToWallet target vl -> payToWallet wllt target vl
    SetSigningProcess sp -> setSigningProcess wllt sp

payToWallet :: forall effs.
    ( Member MultiAgentEffect effs )
    => Wallet
    -> Wallet
    -> Value
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) ()
payToWallet source target amount = void $ fork @effs @EmulatorMessage System High payment
    where payment = walletAction source $ payToPublicKey_ defaultSlotRange amount (EM.walletPubKey target)

setSigningProcess :: forall effs.
    ( Member MultiAgentEffect effs )
    => Wallet
    -> SigningProcess
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) ()
setSigningProcess wllt sp = void $ fork @effs @EmulatorMessage System High st
    where st = walletControlAction wllt $ W.setSigningProcess sp

activate :: forall s e effs.
    ( ContractConstraints s
    , Member ContractInstanceIdEff effs
    , Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    )
    => Wallet
    -> Contract s e ()
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) (ContractHandle s e)
activate wllt con = do
    i <- nextId
    let handle = ContractHandle{chContract=con, chInstanceId = i}
    _ <- fork @effs @EmulatorMessage System High (runReader wllt $ contractThread handle)
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
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) ()
callEndpoint _ ContractHandle{chInstanceId} ep = do
    threadId <- getThread chInstanceId
    let epJson = JSON.toJSON $ Endpoint.event @l @ep @s ep
        thr = void $ mkSysCall @effs @EmulatorMessage High (Message threadId $ EndpointCall epJson)
    void $ fork @effs @EmulatorMessage System High thr

emRunGlobal :: forall b effs.
    EmulatorGlobal b
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) b
emRunGlobal = \case
    WaitUntilSlot s -> go where
        go = sleep @effs Sleeping >>= \case { Just (NewSlot sl) | sl >= s -> pure sl; _ -> go }
