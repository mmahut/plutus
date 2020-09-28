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
    SuspendedThread (..),
    FetchSystemEvent (..),
    EmThread (..),
    Priority(..),
    SimulatorSystemCall(..),
    handleEmulator,
    runSimulator,

    -- * Creating threads
    suspend
  )
where

import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Extras
import           Data.Hashable                 (Hashable)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as Seq
import           GHC.Generics                  (Generic)

class SimulatorBackend a where
  type LocalAction a :: * -> *
  type GlobalAction a :: * -> *
  type Agent a

data Simulator a b where
  RunLocal :: SimulatorBackend a => Agent a -> LocalAction a b -> Simulator a b
  RunGlobal :: SimulatorBackend a => GlobalAction a b -> Simulator a b

newtype ThreadId = ThreadId { unThreadId :: Int }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable)

initialThreadId :: ThreadId
initialThreadId = ThreadId 0

-- | Thread that can be run by the scheduler
data EmThread effs systemEvent =
    EmThread
      { emContinuation :: Maybe systemEvent -> Eff effs (Status effs (SimulatorSystemCall effs systemEvent) (Maybe systemEvent) ())
      , emThreadId     :: ThreadId
      }

-- | Suspended 'EmThread' with information about when it intends to bewoken
--   up again.
data SuspendedThread effs systemEvent
  = SuspendedThread
      { stPrio   :: Priority,
        stThread :: EmThread effs systemEvent
      }

data Priority = Low | High | Sleeping

-- | The "system calls" we can make when interpreting a 'Simulator' action.
data SimulatorSystemCall effs systemEvent
    = Fork (ThreadId -> SuspendedThread effs systemEvent)
    | Suspend Priority -- ^ Yield to other running threads
    | Broadcast systemEvent
    | Message ThreadId systemEvent

newtype FetchSystemEvent effs systemEvent = FetchSystemEvent {runFetchSystemEvent :: Eff effs systemEvent }

data SimulatorInterpreter a effs systemEvent
    = SimulatorInterpreter
        { interpRunLocal     :: forall b. Agent a -> LocalAction a b -> Eff effs (b, ThreadId -> SuspendedThread effs systemEvent)
        , interpRunGlobal    :: forall b. GlobalAction a b -> Eff effs (b, ThreadId -> SuspendedThread effs systemEvent)
        }

handleEmulator ::
  forall a effs systemEvent.
  SimulatorInterpreter a effs systemEvent ->
  Simulator a
    ~> Eff (Yield (SimulatorSystemCall effs systemEvent) (Maybe systemEvent) ': effs)
handleEmulator SimulatorInterpreter {interpRunGlobal, interpRunLocal} = \case
  RunLocal wllt localAction -> do
    (b, thread) <- raise $ interpRunLocal wllt localAction
    _ <- yield @(SimulatorSystemCall effs systemEvent) @(Maybe systemEvent) (Fork thread) id
    pure b
  RunGlobal globalAction -> do
    (b, thread) <- raise $ interpRunGlobal globalAction
    _ <- yield @(SimulatorSystemCall effs systemEvent) @(Maybe systemEvent) (Fork thread) id
    pure b

suspend ::
    Priority
    -> EmThread effs systemEvent
    -> SuspendedThread effs systemEvent
suspend prio thread =
  SuspendedThread
    { stPrio = prio,
      stThread = thread
    }

runSimulator ::
  forall a effs systemEvent.
  SimulatorInterpreter a effs systemEvent ->
  Eff '[Simulator a] () ->
  Eff effs ()
runSimulator i =
  runThreads
    . interpret (handleEmulator i)
    . raiseEnd

runThreads ::
  forall effs systemEvent.
  Eff (Yield (SimulatorSystemCall effs systemEvent) (Maybe systemEvent) ': effs) () ->
  Eff effs ()
runThreads e = do
  k <- runC e
  case k of
    Done () -> pure ()
    Continue _ k' ->
        let initialThread = EmThread{emContinuation = k', emThreadId = initialThreadId}
        in loop $ enqueue (suspend High initialThread) initialState

-- TODO:
-- 1. The sleep argument should always include a priority and only maybe a syscall
-- 2. Implement broadcast / message
-- 3. Put the scheduler in its own module, type parameter for threads

-- | Run the threads that are scheduled in a 'SchedulerState' to completion.
loop :: SchedulerState effs systemEvent -> Eff effs ()
loop s = do
    case dequeue s of
        AThread EmThread{emContinuation, emThreadId} event newState -> do
            result <- emContinuation event
            case result of
                Done _ -> loop newState
                Continue sysCall k -> do
                    let newState'' = case sysCall of
                            Fork thread' ->
                                let (newState', tid) = nextThreadId newState
                                in enqueue (thread' tid) $ enqueue (suspend High EmThread{emThreadId=emThreadId, emContinuation=k}) $ newState'
                            Suspend prio -> enqueue (suspend prio EmThread{emThreadId=emThreadId, emContinuation=k}) newState
                            Broadcast msg -> error "Broadcast"
                            Message t msg -> error "Message"
                    loop newState''
        NoMoreThreads -> pure ()

-- | Scheduler state consisting of three queues of suspended threads, one for each
--   'Priority' level.
data SchedulerState effs systemEvent
    = SchedulerState
        { ssHighPrio  :: Seq (EmThread effs systemEvent)
        , ssLowPrio   :: Seq (EmThread effs systemEvent)
        , ssSleeping  :: Seq (EmThread effs systemEvent)
        , ssThreadId  :: ThreadId
        , ssMailboxes :: HashMap ThreadId (Seq systemEvent)
        }

nextThreadId ::
    SchedulerState effs systemEvent -> (SchedulerState effs systemEvent, ThreadId)
nextThreadId s@SchedulerState{ssThreadId=ThreadId i} = (s{ssThreadId = ThreadId $ succ i}, ThreadId i)

initialState :: SchedulerState effs systemEvent
initialState = SchedulerState Seq.empty Seq.empty Seq.empty (ThreadId 1) HashMap.empty

enqueue :: SuspendedThread effs systemEvent -> SchedulerState effs systemEvent -> SchedulerState effs systemEvent
enqueue SuspendedThread {stPrio, stThread} s =
    case stPrio of
        High     -> s { ssHighPrio = ssHighPrio s Seq.|> stThread }
        Low      -> s { ssLowPrio = ssLowPrio s Seq.|> stThread }
        Sleeping -> s { ssSleeping = ssSleeping s Seq.|> stThread }

-- | Result of calling 'dequeue'. Either a thread that is ready to receive a message,
--   or no more threads.
data SchedulerDQResult effs systemEvent
    = AThread (EmThread effs systemEvent) (Maybe systemEvent) (SchedulerState effs systemEvent)
    | NoMoreThreads

dequeue :: SchedulerState effs systemEvent -> SchedulerDQResult effs systemEvent
dequeue s = case dequeueThread s of
    Nothing -> NoMoreThreads
    Just (s', thread) -> case dequeueMessage s' (emThreadId thread) of
        Nothing       -> AThread thread Nothing s'
        Just (s'', m) -> AThread thread (Just m) s''

dequeueThread :: SchedulerState effs systemEvent -> Maybe (SchedulerState effs systemEvent, EmThread effs systemEvent)
dequeueThread s@SchedulerState { ssHighPrio, ssLowPrio, ssSleeping } =
    case Seq.viewl ssHighPrio of
        x Seq.:< xs -> Just (s{ssHighPrio = xs}, x)
        Seq.EmptyL -> case Seq.viewl ssLowPrio of
            x Seq.:< xs -> Just (s{ssLowPrio=xs}, x)
            Seq.EmptyL -> case Seq.viewl ssSleeping of
                x Seq.:< xs -> Just (s{ssSleeping=xs}, x)
                Seq.EmptyL  -> Nothing

dequeueMessage :: SchedulerState effs systemEvent -> ThreadId -> Maybe (SchedulerState effs systemEvent, systemEvent)
dequeueMessage s@SchedulerState{ssMailboxes} i = do
    mailbox <- HashMap.lookup i ssMailboxes
    (x, xs) <- case Seq.viewl mailbox of { Seq.EmptyL -> Nothing; x Seq.:< xs -> Just (x, xs) }
    let mbs = HashMap.insert i xs ssMailboxes
    pure (s{ssMailboxes = mbs}, x)
