{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Types
  ( SimulatorBackend (..),
    Simulator (..),

    -- * Handling the 'Simulator' effect
    SimulatorInterpreter (..),
    handleSimulator,
    runSimulator,
  )
where

import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Extras
import           Plutus.Trace.Scheduler

class SimulatorBackend a where
    type LocalAction a :: * -> *
    type GlobalAction a :: * -> *
    type Agent a

data Simulator a b where
    RunLocal :: SimulatorBackend a => Agent a -> LocalAction a b -> Simulator a b
    RunGlobal :: SimulatorBackend a => GlobalAction a b -> Simulator a b

data SimulatorInterpreter a effs systemEvent
    = SimulatorInterpreter
        { _runLocal     :: forall b. Agent a -> LocalAction a b -> Eff (Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) b
        , _runGlobal    :: forall b. GlobalAction a b -> Eff (Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) b
        }

handleSimulator ::
    forall a effs systemEvent.
    SimulatorInterpreter a effs systemEvent
    -> Simulator a
    ~> Eff (Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs)
handleSimulator SimulatorInterpreter {_runGlobal, _runLocal} = \case
    RunLocal wllt localAction -> _runLocal wllt localAction
    RunGlobal globalAction -> _runGlobal globalAction

runSimulator ::
    forall a effs systemEvent.
    Eq systemEvent
    => SimulatorInterpreter a effs systemEvent
    -> Eff '[Simulator a] ()
    -> Eff effs ()
runSimulator i = runThreads . interpret (handleSimulator i) . raiseEnd
