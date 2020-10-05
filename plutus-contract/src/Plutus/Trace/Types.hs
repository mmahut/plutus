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
    EmThread (..),
    Priority(..),
    WithPriority(..),
    SimulatorSystemCall,
    SuspendedThread,
    handleEmulator,
    runSimulator,

    -- * Creating threads
    suspend
  )
where

import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Extras
import Control.Lens
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

data Priority = Low | High | Sleeping
    deriving stock (Eq, Ord, Show, Generic)

-- | The "system calls" we can make when interpreting a 'Simulator' action.
data SysCall effs systemEvent
    = Fork (ThreadId -> SuspendedThread effs systemEvent)
    | Suspend
    | Broadcast systemEvent
    | Message ThreadId systemEvent

-- | A suspended thread with a 'Priority'.
data WithPriority t
    = WithPriority
        { _priority :: Priority
        , _thread   :: t
        }

type SimulatorSystemCall effs systemEvent = WithPriority (SysCall effs systemEvent)
type SuspendedThread effs systemEvent = WithPriority (EmThread effs systemEvent)

-- | Thread that can be run by the scheduler
data EmThread effs systemEvent =
    EmThread
        { _continuation :: Maybe systemEvent -> Eff effs (Status effs (SimulatorSystemCall effs systemEvent) (Maybe systemEvent) ())
        , _threadId     :: ThreadId
        }

-- | Scheduler state consisting of three queues of suspended threads, one for each
--   'Priority' level.
data SchedulerState effs systemEvent
    = SchedulerState
        { _highPrio  :: Seq (EmThread effs systemEvent)
        , _lowPrio   :: Seq (EmThread effs systemEvent)
        , _sleeping  :: Seq (EmThread effs systemEvent)
        , _lastThreadId  :: ThreadId
        , _mailboxes :: HashMap ThreadId (Seq systemEvent)
        }

makeLenses ''SchedulerState

data SimulatorInterpreter a effs systemEvent
    = SimulatorInterpreter
        { _runLocal     :: forall b. Agent a -> LocalAction a b -> Eff effs (b, ThreadId -> SuspendedThread effs systemEvent)
        , _runGlobal    :: forall b. GlobalAction a b -> Eff effs (b, ThreadId -> SuspendedThread effs systemEvent)
        }

handleEmulator ::
    forall a effs systemEvent.
    SimulatorInterpreter a effs systemEvent
    -> Simulator a
    ~> Eff (Yield (SimulatorSystemCall effs systemEvent) (Maybe systemEvent) ': effs)
handleEmulator SimulatorInterpreter {_runGlobal, _runLocal} = \case
    RunLocal wllt localAction -> do
        (b, thread) <- raise $ _runLocal wllt localAction
        _ <- yield @(SimulatorSystemCall effs systemEvent) @(Maybe systemEvent) (WithPriority Low $ Fork thread) id
        pure b
    RunGlobal globalAction -> do
        (b, thread) <- raise $ _runGlobal globalAction
        _ <- yield @(SimulatorSystemCall effs systemEvent) @(Maybe systemEvent) (WithPriority Low $ Fork thread) id
        pure b

suspend ::
    Priority
    -> EmThread effs systemEvent
    -> SuspendedThread effs systemEvent
suspend = WithPriority

runSimulator ::
    forall a effs systemEvent.
    Eq systemEvent
    => SimulatorInterpreter a effs systemEvent
    -> Eff '[Simulator a] ()
    -> Eff effs ()
runSimulator i = runThreads . interpret (handleEmulator i) . raiseEnd

runThreads ::
    forall effs systemEvent.
    Eq systemEvent
    => Eff (Yield (SimulatorSystemCall effs systemEvent) (Maybe systemEvent) ': effs) ()
    -> Eff effs ()
runThreads e = do
    k <- runC e
    case k of
        Done () -> pure ()
        Continue _ k' ->
            let initialThread = EmThread{_continuation = k', _threadId = initialThreadId}
            in loop $ enqueue (suspend High initialThread) initialState

-- | Run the threads that are scheduled in a 'SchedulerState' to completion.
loop :: Eq systemEvent => SchedulerState effs systemEvent -> Eff effs ()
loop s = do
    case dequeue s of
        AThread EmThread{_continuation, _threadId} event schedulerState -> do
            result <- _continuation event
            case result of
                Done _ -> loop schedulerState
                Continue WithPriority{_priority, _thread=sysCall} k -> do
                    let thisThread = suspend _priority EmThread{_threadId=_threadId, _continuation=k}
                        updatedState = case sysCall of
                            Fork newThread ->
                                let (schedulerState', tid) = nextThreadId schedulerState
                                in enqueue (newThread tid) schedulerState'
                            Suspend -> schedulerState
                            Broadcast msg -> schedulerState & mailboxes . traversed %~ (|> msg)
                            Message t msg -> schedulerState & mailboxes . at t . non mempty %~ (|> msg)
                    loop (updatedState & enqueue thisThread)
        NoMoreThreads -> pure ()

nextThreadId ::
    SchedulerState effs systemEvent -> (SchedulerState effs systemEvent, ThreadId)
nextThreadId s = (s & lastThreadId %~ ThreadId . succ . unThreadId, s ^. lastThreadId)

initialState :: SchedulerState effs systemEvent
initialState = SchedulerState Seq.empty Seq.empty Seq.empty (ThreadId 1) HashMap.empty

enqueue :: SuspendedThread effs systemEvent -> SchedulerState effs systemEvent -> SchedulerState effs systemEvent
enqueue WithPriority {_priority, _thread} s =
    case _priority of
        High     -> s & highPrio %~ (|> _thread)
        Low      -> s & lowPrio %~ (|> _thread)
        Sleeping -> s & sleeping %~ (|> _thread)

-- | Result of calling 'dequeue'. Either a thread that is ready to receive a message,
--   or no more threads.
data SchedulerDQResult effs systemEvent
    = AThread (EmThread effs systemEvent) (Maybe systemEvent) (SchedulerState effs systemEvent)
    | NoMoreThreads

dequeue :: SchedulerState effs systemEvent -> SchedulerDQResult effs systemEvent
dequeue s = case dequeueThread s of
    Nothing -> NoMoreThreads
    Just (s', thread) -> case dequeueMessage s' (_threadId thread) of
        Nothing       -> AThread thread Nothing s'
        Just (s'', m) -> AThread thread (Just m) s''

dequeueThread :: SchedulerState effs systemEvent -> Maybe (SchedulerState effs systemEvent, EmThread effs systemEvent)
dequeueThread s =
    case s ^. highPrio . to Seq.viewl of
        x Seq.:< xs -> Just (s & highPrio .~ xs, x)
        Seq.EmptyL -> case s ^. lowPrio . to Seq.viewl of
            x Seq.:< xs -> Just (s & lowPrio .~ xs, x)
            Seq.EmptyL -> case s ^. sleeping . to Seq.viewl of
                x Seq.:< xs -> Just (s & sleeping .~ xs, x)
                Seq.EmptyL  -> Nothing

dequeueMessage :: SchedulerState effs systemEvent -> ThreadId -> Maybe (SchedulerState effs systemEvent, systemEvent)
dequeueMessage s i = do
    mailbox <- s ^. mailboxes . at i
    (x, xs) <- case Seq.viewl mailbox of { Seq.EmptyL -> Nothing; x Seq.:< xs -> Just (x, xs) }
    pure (s & mailboxes . at i .~ Just xs, x)
