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
    -- * Interpreter
    , interpretEmulator
    -- * Instance IDs
    , ContractInstanceIdEff(..)
    , uniqueId
    ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Reader             (runReader)
import           Control.Monad.Freer.State              (State)
import           Control.Monad.Freer.TH                 (makeEffect)
import           Language.Plutus.Contract               (Contract)
import           Plutus.Trace.Scheduler                 (Priority (..), SuspendedThread, ThreadId, mkThread)
import           Wallet.Emulator.MultiAgent             (MultiAgentEffect)
import           Wallet.Emulator.Wallet                 (Wallet (..))
import           Wallet.Types                           (ContractInstanceId)

import           Plutus.Trace.Emulator.ContractInstance (contractThread)
import           Plutus.Trace.Emulator.Types            (ContractConstraints, ContractHandle (..), Emulator,
                                                         EmulatorEvent, EmulatorGlobal (..), EmulatorLocal (..),
                                                         EmulatorState)
import           Plutus.Trace.Types

data ContractInstanceIdEff r where
    UniqueId :: ContractInstanceIdEff ContractInstanceId
makeEffect ''ContractInstanceIdEff

interpretEmulator :: forall effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    , Member MultiAgentEffect effs
    )
    => SimulatorInterpreter Emulator effs EmulatorEvent
interpretEmulator = SimulatorInterpreter
    { _runLocal = emRunLocal
    , _runGlobal = emRunGlobal
    }

emRunLocal :: forall b effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    , Member MultiAgentEffect effs
    )
    => Wallet
    -> EmulatorLocal b
    -> Eff effs (b, ThreadId -> SuspendedThread effs EmulatorEvent)
emRunLocal wllt = \case
    ActivateContract con -> activate wllt con

activate ::
    ( ContractConstraints s
    , Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    , Member MultiAgentEffect effs
    )
    => Wallet
    -> Contract s e ()
    -> Eff effs (ContractHandle s e, ThreadId -> SuspendedThread effs EmulatorEvent)
activate wllt con = do
    i <- uniqueId
    let handle = ContractHandle{chContract=con, chInstanceId = i}
    pure (handle, mkThread High (runReader wllt $ contractThread handle))

emRunGlobal :: forall b effs. EmulatorGlobal b -> Eff effs (b, ThreadId -> SuspendedThread effs EmulatorEvent)
emRunGlobal = undefined

