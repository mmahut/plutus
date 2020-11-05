{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Plutus.Trace.Playground(
    Playground
    , PlaygroundLocal(..)
    , EmulatorGlobal(..)
    , waitUntilSlot
    , waitNSlots
    , payToWallet
    , callEndpoint
    , interpretPlaygroundTrace
    , runPlaygroundStream
    , walletInstanceTag
    , EmulatorConfig(..)
    , defaultEmulatorConfig
    ) where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer (Member, Eff, interpret, reinterpret, send)
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (runReader)
import           Control.Monad.Freer.Extras                      (raiseEnd)
import           Control.Monad.Freer.Log                         (LogMsg (..), mapLog, LogMessage)
import Control.Monad.Freer.State (State, evalState, modify, gets)
import           Control.Monad.Freer.Coroutine                   (Yield)
import qualified Data.Aeson             as JSON
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString(..))
import Ledger.Slot (Slot(..))
import           Ledger.Value           (Value)
import           Numeric.Natural                    (Natural)

import           Language.Plutus.Contract                      (Contract (..), HasBlockchainActions)
import           Wallet.Emulator.Wallet (Wallet (..))
import Plutus.Trace.Emulator.Types (walletInstanceTag)
import           Plutus.Trace.Effects.ContractInstanceId         (ContractInstanceIdEff, handleDeterministicIds, nextId)
import           Plutus.Trace.Scheduler                          (Priority (..), SysCall (..), SystemCall,
                                                                  fork, mkSysCall, runThreads)
import           Wallet.API                                      (payToPublicKey, defaultSlotRange)
import qualified Wallet.Emulator                                 as EM
import Wallet.Emulator.Stream (runTraceStream, EmulatorConfig(..), EmulatorErr(..), initialDistribution, defaultEmulatorConfig)
import           Wallet.Emulator.Chain                           (ChainControlEffect, ChainEffect)
import qualified Wallet.Emulator.Chain              as ChainState
import           Wallet.Emulator.MultiAgent                      (EmulatorEvent' (..), EmulatorState,
                                                                  MultiAgentEffect,
                                                                  schedulerEvent, walletAction, EmulatorEvent)
import Wallet.Types (ContractInstanceId)
import           Wallet.Emulator.Chain              (ChainState)
import           Plutus.Trace.Types
import           Plutus.Trace.Emulator.ContractInstance          (ContractInstanceError, contractThread, getThread)
import           Plutus.Trace.Emulator.System                    (launchSystemThreads)
import qualified Plutus.Trace.Emulator as Emulator
import           Plutus.Trace.Emulator.Types                     (ContractConstraints, ContractHandle (..),
                                                                  EmulatorMessage (..), EmulatorThreads, ContractInstanceError(..), EmulatorGlobal(..), ContractInstanceTag)
import Streaming (Stream)
import Streaming.Prelude (Of)

{- Note [Wallet contract instances]

In the Playground we have a single 'Contract' that we are testing, and each
wallet runs exactly one instance of this contract. As a result,

1. The 'PlaygroundLocal' actions don't require any references to a contract
   instance because there is only one per wallet
2. We don't need an @ActivateContract@ action, we can just start all the 
   instances at the beginning of the simulation, using 'launchContract'
3. For each wallet there is a unique 'ContractInstanceTag' identifying the
   wallet's contract instance. Defined in 'walletInstanceTag'. This is
   useful for the instance-specific folds in 'Wallet.Emulator.Folds'.

-}

data Playground

-- | Local actions available in the Playgroun.
--   See note [Wallet contract instances].
data PlaygroundLocal r where
   CallEndpoint :: String -> JSON.Value -> PlaygroundLocal ()
   PayToWallet :: Wallet -> Value -> PlaygroundLocal ()

instance TraceBackend Playground where
    type LocalAction Playground = PlaygroundLocal
    type GlobalAction Playground = EmulatorGlobal
    type Agent Playground = Wallet

-- | Look at the 'ChainState'
chainState :: Eff '[Trace Playground] ChainState
chainState = send @(Trace Playground) $ RunGlobal ChainState

-- | Wait until the simulation has reached the given slot
waitUntilSlot :: Slot -> Eff '[Trace Playground] Slot
waitUntilSlot sl = send @(Trace Playground) $ RunGlobal (WaitUntilSlot sl)

-- | Wait for a number of slots
waitNSlots :: Natural -> Eff '[Trace Playground] Slot
waitNSlots n = do
    Slot c <- view ChainState.currentSlot <$> chainState
    waitUntilSlot (Slot $ c + fromIntegral n)

-- | Pay some funds from one wallet to another with the given validity range
payToWallet :: Wallet -> Wallet -> Value -> Eff '[Trace Playground] ()
payToWallet from_ to_ = send @(Trace Playground) . RunLocal from_ . PayToWallet  to_

callEndpoint :: Wallet -> String -> JSON.Value -> Eff '[Trace Playground] ()
callEndpoint wllt ep = send @(Trace Playground) . RunLocal wllt . CallEndpoint ep

-- | Run a 'Trace Playground', streaming the log messages as they arrive
runPlaygroundStream :: forall s e effs a.
    ( HasBlockchainActions s
    , ContractConstraints s
    )
    => EmulatorConfig
    -> Contract s e ()
    -> Eff '[Trace Playground] a
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr)
runPlaygroundStream conf contract = runTraceStream conf . interpretPlaygroundTrace contract (conf ^. initialDistribution . to Map.keys)

interpretPlaygroundTrace :: forall s e effs a.
    ( Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member ChainEffect effs
    , Member ChainControlEffect effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (State EmulatorState) effs
    , HasBlockchainActions s
    , ContractConstraints s
    )
    => Contract s e () -- ^ The contract
    -> [Wallet] -- ^ Wallets that should be simulated in the emulator
    -> Eff '[Trace Playground] a
    -> Eff effs ()
interpretPlaygroundTrace contract wallets action =
    evalState @EmulatorThreads mempty
        $ evalState @(Map Wallet ContractInstanceId) Map.empty
        $ handleDeterministicIds
        $ interpret (mapLog (review schedulerEvent))
        $ runThreads
        $ do
            launchSystemThreads wallets
            traverse_ (launchContract contract) wallets
            interpret (handleTrace plInterpreter) $ void $ raiseEnd action

-- | Start the wallet's instance and
--   register the 'ContractInstanceId' in the map of wallets.
--   See note [Wallet contract instances].
launchContract :: forall s e effs.
    ( HasBlockchainActions s
    , ContractConstraints s
    , Member ContractInstanceIdEff effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (Error ContractInstanceError) effs
    , Member (State EmulatorThreads) effs
    , Member (State (Map Wallet ContractInstanceId)) effs
    , Member MultiAgentEffect effs
    )
    => Contract s e ()
    -> Wallet
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) ()
launchContract contract wllt = do
    i <- nextId
    let handle = ContractHandle{chContract=contract, chInstanceId = i, chInstanceTag = walletInstanceTag wllt}
    void $ fork @effs @EmulatorMessage "contract instance" High (runReader wllt $ interpret (mapLog InstanceEvent) $ reinterpret (mapLog InstanceEvent) $ contractThread handle)
    modify @(Map Wallet ContractInstanceId) (set (at wllt) (Just i))

plInterpreter :: forall effs.
    ( Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member (State EmulatorState) effs
    , Member (State (Map Wallet ContractInstanceId)) effs
    )
    => TraceInterpreter Playground effs EmulatorMessage
plInterpreter = TraceInterpreter
    { _runLocal = plRunLocal
    , _runGlobal = Emulator.emRunGlobal
    }

plRunLocal :: forall b effs.
    ( Member (State EmulatorThreads) effs
    , Member MultiAgentEffect effs
    , Member (Error ContractInstanceError) effs
    , Member (State (Map Wallet ContractInstanceId)) effs
    )
    => Wallet
    -> PlaygroundLocal b
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) b
plRunLocal wllt = \case
    CallEndpoint name value -> handleCallEndpoint wllt name value
    PayToWallet target amount -> walletAction wllt $ void $ payToPublicKey defaultSlotRange amount (EM.walletPubKey target)

getInstance ::
    ( Member (State (Map Wallet ContractInstanceId)) effs
    , Member (Error ContractInstanceError) effs
    )
    => Wallet
    -> Eff effs ContractInstanceId
getInstance wllt = do
    r <- gets @(Map Wallet ContractInstanceId) (view (at wllt)) 
    case r of
        Nothing -> throwError (InstanceIdNotFound wllt)
        Just i -> pure i

handleCallEndpoint :: forall effs.
    ( Member (Error ContractInstanceError) effs
    , Member (State (Map Wallet ContractInstanceId)) effs
    , Member (State EmulatorThreads) effs
    )
    => Wallet
    -> String
    -> JSON.Value
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) ()
handleCallEndpoint wllt endpointName endpointValue = do
    let epJson = JSON.object ["tag" JSON..= endpointName, "value" JSON..= endpointValue]
        thr = do
            threadId <- getInstance wllt >>= getThread
            void $ mkSysCall @effs @EmulatorMessage High (Message threadId $ EndpointCall epJson)
    void $ fork @effs @EmulatorMessage "call endpoint" Low thr