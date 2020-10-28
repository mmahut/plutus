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
  ( TraceBackend (..),
    Trace (..),

    -- * Handling the 'Trace' effect
    TraceInterpreter (..),
    handleTrace,
    runTrace,

    -- * Logging
    SchedulerLog(..),
    ThreadEvent(..)
  )
where

import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Extras
import           Control.Monad.Freer.Log       (LogMsg)
import           Plutus.Trace.Scheduler
import Data.Text (Text)

class TraceBackend a where
    type LocalAction a :: * -> *
    type GlobalAction a :: * -> *
    type Agent a

data Trace a b where
    RunLocal :: TraceBackend a => Agent a -> LocalAction a b -> Trace a b
    RunGlobal :: TraceBackend a => GlobalAction a b -> Trace a b

data TraceInterpreter a effs systemEvent
    = TraceInterpreter
        { _runLocal     :: forall b. Agent a -> LocalAction a b -> Eff (Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) b
        , _runGlobal    :: forall b. GlobalAction a b -> Eff (Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) b
        }

handleTrace ::
    forall a effs systemEvent.
    TraceInterpreter a effs systemEvent
    -> Trace a
    ~> Eff (Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs)
handleTrace TraceInterpreter {_runGlobal, _runLocal} = \case
    RunLocal wllt localAction -> _runLocal wllt localAction
    RunGlobal globalAction -> _runGlobal globalAction

runTrace ::
    forall a effs systemEvent.
    ( Eq systemEvent
    , Member (LogMsg SchedulerLog) effs
    )
    => TraceInterpreter a effs systemEvent
    -> Eff '[Trace a] ()
    -> Eff effs ()
runTrace i = runThreads . interpret (handleTrace i) . raiseEnd
