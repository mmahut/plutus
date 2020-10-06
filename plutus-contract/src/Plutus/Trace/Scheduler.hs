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

module Plutus.Trace.Scheduler(
    ThreadId
    , SysCall(..)
    , WithPriority(..)
    , Priority(..)
    , SystemCall
    , SuspendedThread
    , EmThread(..)
    , SchedulerState(..)
    , runThreads
    , mkThread
    , mkSysCall
    , suspend
    ) where


import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Reader
import           Data.Hashable                 (Hashable)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as Seq
import           GHC.Generics                  (Generic)

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

type SystemCall effs systemEvent = WithPriority (SysCall effs systemEvent)
type SuspendedThread effs systemEvent = WithPriority (EmThread effs systemEvent)

-- | Thread that can be run by the scheduler
data EmThread effs systemEvent =
    EmThread
        { _continuation :: Maybe systemEvent -> Eff effs (Status effs (SystemCall effs systemEvent) (Maybe systemEvent) ())
        , _threadId     :: ThreadId
        }

-- | Scheduler state consisting of three queues of suspended threads, one for
--   each 'Priority' level.
data SchedulerState effs systemEvent
    = SchedulerState
        { _highPrio     :: Seq (EmThread effs systemEvent)
        , _lowPrio      :: Seq (EmThread effs systemEvent)
        , _sleeping     :: Seq (EmThread effs systemEvent)
        , _lastThreadId :: ThreadId
        , _mailboxes    :: HashMap ThreadId (Seq systemEvent)
        }

makeLenses ''SchedulerState

suspend :: Priority -> EmThread effs systemEvent -> SuspendedThread effs systemEvent
suspend = WithPriority

-- | Make a thread with the given priority from an action. This is a
--   convenience for defining 'SimulatorInterpreter' values.
mkThread :: Priority -> Eff (Reader ThreadId ': Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) () -> ThreadId -> SuspendedThread effs systemEvent
mkThread prio action tid =
    let action' = runReader tid action
    in WithPriority
            { _priority = prio
            , _thread = EmThread
                { _threadId = tid
                , _continuation = \_ -> runC action'
                }
            }

mkSysCall :: forall effs systemEvent effs2.
    Member (Yield (SystemCall effs systemEvent) (Maybe systemEvent)) effs2
    => Priority
    -> SysCall effs systemEvent
    -> Eff effs2 (Maybe systemEvent)
mkSysCall prio sc = yield @(SystemCall effs systemEvent) @(Maybe systemEvent) (WithPriority prio sc) id

runThreads ::
    forall effs systemEvent.
    Eq systemEvent
    => Eff (Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) ()
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
                    loop (schedulerState & handleSysCall sysCall & enqueue thisThread)
        NoMoreThreads -> pure ()

handleSysCall ::
    Eq systemEvent
    => SysCall effs systemEvent
    -> SchedulerState effs systemEvent
    -> SchedulerState effs systemEvent
handleSysCall sysCall schedulerState = case sysCall of
    Fork newThread ->
        let (schedulerState', tid) = nextThreadId schedulerState
        in enqueue (newThread tid) schedulerState'
    Suspend -> schedulerState
    Broadcast msg -> schedulerState & mailboxes . traversed %~ (|> msg)
    Message t msg -> schedulerState & mailboxes . at t . non mempty %~ (|> msg)

nextThreadId :: SchedulerState effs systemEvent -> (SchedulerState effs systemEvent, ThreadId)
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
