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
    , defaultEmulatorConfig
    , runEmulatorTrace
    -- * Interpreter
    , interpretEmulatorTrace
    , emInterpreter
    -- * Testing
    , runTest
    ) where

import           Control.Lens
import           Control.Monad                                   (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine                   (Yield)
import           Control.Monad.Freer.Error                       (Error, runError)
import           Control.Monad.Freer.Extras                      (raiseEnd, raiseEnd4, raiseEnd6, wrapError,
                                                                  writeIntoState)
import           Control.Monad.Freer.Log                         (LogLevel (..), LogMessage (..), LogMsg (..),
                                                                  handleLogWriter, mapLog, mapMLog)
import           Control.Monad.Freer.Reader                      (runReader)
import           Control.Monad.Freer.State                       (State, evalState, gets, runState)
import           Control.Monad.Freer.Writer                      (Writer, tell)
import qualified Data.Aeson                                      as JSON
import           Data.Foldable                                   (traverse_)
import qualified Data.Map                                        as Map
import           Data.Maybe                                      (fromMaybe)
import           Data.Proxy                                      (Proxy)
import           Data.Text.Prettyprint.Doc                       (Pretty (pretty))
import           Language.Plutus.Contract                        (Contract, HasEndpoint)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoint
import           Ledger.Slot                                     (Slot (..))
import           Ledger.Value                                    (Value)
import           Plutus.Trace.Scheduler                          (Priority (..), SysCall (..), SystemCall,
                                                                  ThreadType (..), fork, mkSysCall, runThreads, sleep)
import           Wallet.API                                      (WalletAPIError, defaultSlotRange, payToPublicKey_)
import qualified Wallet.Emulator                                 as EM
import           Wallet.Emulator.Chain                           (ChainControlEffect, ChainEffect, getCurrentSlot)
import qualified Wallet.Emulator.Chain                           as ChainState
import           Wallet.Emulator.MultiAgent                      (EmulatorEvent, EmulatorEvent' (..), EmulatorState,
                                                                  EmulatorTimeEvent (..), MultiAgentEffect, emulatorLog,
                                                                  emulatorTimeEvent, schedulerEvent, walletAction,
                                                                  walletControlAction)
import           Wallet.Emulator.Wallet                          (SigningProcess, Wallet (..))
import qualified Wallet.Emulator.Wallet                          as Wallet
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


data EmulatorConfig =
    EmulatorConfig
        { _InitialDistribution :: InitialDistribution
        }

defaultEmulatorConfig :: EmulatorConfig
defaultEmulatorConfig =
    EmulatorConfig
        { _InitialDistribution = defaultDist
        }

makeLenses ''EmulatorConfig

-- | Run a 'Trace Emulator', returning the final state and possibly an error
runEmulatorTrace :: EmulatorConfig -> Eff '[Trace Emulator] a -> (Either EmulatorErr (), EM.EmulatorState)
runEmulatorTrace conf = runTraceBackend conf . interpretEmulatorTrace (conf ^. initialDistribution . to Map.keys)

data EmulatorErr =
    WalletErr WalletAPIError
    | AssertionErr EM.AssertionError
    | InstanceErr ContractInstanceError
    deriving (Show)

runTraceBackend ::
    EmulatorConfig
    -> Eff '[ State EmulatorState
            , LogMsg EmulatorEvent'
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
    . subsume
    . subsume @(State EmulatorState)
    . raiseEnd6

-- | Annotate emulator log messages with the current system time
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

initialState :: EmulatorConfig -> EM.EmulatorState
initialState EmulatorConfig{_InitialDistribution} = EM.emulatorStateInitialDist (Map.mapKeys EM.walletPubKey _InitialDistribution)

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
    ActivateContract con -> activate wllt con
    CallEndpointEm p h v -> callEndpoint p h v
    PayToWallet target vl -> payToWallet wllt target vl
    SetSigningProcess sp -> setSigningProcess wllt sp
    AgentState -> gets @EmulatorState (fromMaybe (Wallet.emptyWalletState wllt) . view (EM.walletStates . at wllt))

payToWallet :: forall effs.
    ( Member MultiAgentEffect effs )
    => Wallet
    -> Wallet
    -> Value
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) ()
payToWallet source target amount = void $ fork @effs @EmulatorMessage User High payment
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
    , Member (LogMsg EmulatorEvent') effs
    )
    => Wallet
    -> Contract s e ()
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) (ContractHandle s e)
activate wllt con = do
    i <- nextId
    let handle = ContractHandle{chContract=con, chInstanceId = i}
    _ <- fork @effs @EmulatorMessage System High (runReader wllt $ interpret (mapLog InstanceEvent) $ reinterpret (mapLog InstanceEvent) $ contractThread handle)
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
    Member (State EmulatorState) effs
    => EmulatorGlobal b
    -> Eff (Yield (SystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) b
emRunGlobal = \case
    WaitUntilSlot s -> go where
        go = sleep @effs Sleeping >>= \case { Just (NewSlot sl) | sl >= s -> pure sl; _ -> go }
    ChainState  -> gets (view EM.chainState)

runTest :: forall a.
    Eff '[Trace Emulator] a
    -> IO ()
runTest action = do
    let result = runEmulatorTrace defaultEmulatorConfig (Types.waitNSlots 1 >> action)
    traverse_ (print . pretty) (filter (\LogMessage{_logLevel} -> _logLevel >= Info) $ (result ^. _2 . emulatorLog))
    print $ fst result

