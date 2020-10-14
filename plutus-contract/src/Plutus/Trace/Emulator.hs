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

import           Control.Monad                                   (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine                   (Yield)
import           Control.Monad.Freer.Error                       (Error, runError)
import           Control.Monad.Freer.Extras                      (raiseEnd, wrapError, raiseEnd4)
import           Control.Monad.Freer.Reader                      (runReader)
import           Control.Monad.Freer.State                       (State, evalState, runState)
import qualified Data.Aeson                                      as JSON
import           Data.Proxy                                      (Proxy)
import           Language.Plutus.Contract                        (Contract, HasEndpoint)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoint
import           Ledger.Value                                    (Value)
import qualified Data.Map as Map
import           Plutus.Trace.Scheduler                          (Priority (..), SysCall (..), SystemCall, fork,
                                                                  mkSysCall, runThreads, sleep, ThreadType(..))
import           Wallet.API                                      (defaultSlotRange, payToPublicKey_, WalletAPIError)
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
import Language.Plutus.Contract.Trace (InitialDistribution, defaultDist)
import qualified Debug.Trace as Trace


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
    -> Eff '[ MultiAgentEffect
            , ChainEffect
            , ChainControlEffect
            , Error ContractInstanceError
            ] ()
    -> (Either EmulatorErr (), EM.EmulatorState)
runTraceBackend conf =
    run
    . runState (initialState conf)
    . runError
    . wrapError WalletErr
    . wrapError AssertionErr
    . wrapError InstanceErr
    . EM.processEmulated
    . raiseEnd4

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
    => TraceInterpreter Emulator effs EmulatorEvent
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
payToWallet source target amount = void $ fork @effs @EmulatorEvent User High payment
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
    _ <- fork @effs @EmulatorEvent System High (runReader wllt $ contractThread handle)
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
    void $ fork @effs @EmulatorEvent System High thr

emRunGlobal :: forall b effs.
    EmulatorGlobal b
    -> Eff (Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent) ': effs) b
emRunGlobal = \case
    WaitUntilSlot s -> go where
        go = sleep @effs Sleeping >>= \case { Just (NewSlot sl) | sl >= s -> pure sl; _ -> go }
