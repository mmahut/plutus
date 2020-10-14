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
    , ThreadType(..)
    , SystemCall
    , SuspendedThread
    , EmThread(..)
    , SchedulerState(..)
    -- * Thread API
    , runThreads
    , fork
    , sleep
    -- * Etc.
    , mkThread
    , mkSysCall
    ) where


import           Control.Lens hiding (Empty)
import Data.Map (Map)
import Data.Map as Map
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Reader
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable                 (Hashable)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Sequence                 (Seq(..))
import qualified Data.Sequence                 as Seq
import           GHC.Generics                  (Generic)
import qualified Debug.Trace as Trace

newtype ThreadId = ThreadId { unThreadId :: Int }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable)

initialThreadId :: ThreadId
initialThreadId = ThreadId 0

data Priority = Low | High | Sleeping
    deriving stock (Eq, Ord, Show, Generic)

data ThreadType = System | User
    deriving stock (Eq, Ord, Show, Generic)

-- | The "system calls" we can make when a 'Simulator' action.
data SysCall effs systemEvent
    = Fork ThreadType (ThreadId -> SuspendedThread effs systemEvent)
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
        , _activeThreads :: Map ThreadType (HashSet ThreadId)
        }

makeLenses ''SchedulerState

desc :: SchedulerState effs systemEvent -> String
desc SchedulerState{_highPrio, _lowPrio, _sleeping} =
    show $ fmap _threadId $ (_highPrio <> _lowPrio <> _sleeping)
    -- "[High priority: "
    --     <> show (_threadId <$> _highPrio)
    --     <> "; low priority: "
    --     <> show (_threadId <$> _lowPrio) 
    --     <> "; sleeping: "
    --     <> show (_threadId <$> _sleeping)
    --     <> "; mailboxes: "
    --     <> show (fmap Seq.length _mailboxes)
    --     <> "; active threads: "
    --     <> show _activeThreads
    --     <> "]"

numThreads :: SchedulerState effs systemEvent -> Int
numThreads SchedulerState{_highPrio, _lowPrio, _sleeping} =
    Seq.length $ _highPrio <> _lowPrio <> _sleeping

removeActiveThread :: ThreadId -> SchedulerState effs systemEvent -> SchedulerState effs systemEvent
removeActiveThread tid = over (activeThreads . mapped) (HashSet.delete tid)

hasActiveUserThreads :: SchedulerState effs systemEvent -> Bool
hasActiveUserThreads = not . view (activeThreads . at User . non mempty . to HashSet.null)

suspendThread :: Priority -> EmThread effs systemEvent -> SuspendedThread effs systemEvent
suspendThread = WithPriority

-- | Make a thread with the given priority from an action. This is a
--   convenience for defining 'SimulatorInterpreter' values.
mkThread :: Priority -> Eff (Reader ThreadId ': Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) () -> ThreadId -> SuspendedThread effs systemEvent
mkThread prio action tid =
    let action' = runReader tid action
    in WithPriority
            { _priority = prio
            , _thread = EmThread
                { _threadId = tid
                , _continuation = Trace.trace ("mkThread: " <> show tid) (\_ -> runC action')
                }
            }

mkSysCall :: forall effs systemEvent effs2.
    Member (Yield (SystemCall effs systemEvent) (Maybe systemEvent)) effs2
    => Priority
    -> SysCall effs systemEvent
    -> Eff effs2 (Maybe systemEvent)
mkSysCall prio sc = yield @(SystemCall effs systemEvent) @(Maybe systemEvent) (Trace.trace "mkSysCall" $ WithPriority prio sc) id

-- | Start a new thread with the given priority
fork :: forall effs systemEvent effs2.
    Member (Yield (SystemCall effs systemEvent) (Maybe systemEvent)) effs2
    => ThreadType
    -> Priority
    -> Eff (Reader ThreadId ': Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) ()
    -> Eff effs2 (Maybe systemEvent)
fork tp prio action = Trace.trace "FORK" (mkSysCall prio (Fork tp $ mkThread prio action))

sleep :: forall effs systemEvent effs2.
    Member (Yield (SystemCall effs systemEvent) (Maybe systemEvent)) effs2
    => Priority
    -> Eff effs2 (Maybe systemEvent)
sleep prio = mkSysCall @effs @systemEvent @effs2 prio Suspend

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
            in loop
                $ initialState
                    & activeThreads . at User . non mempty %~ HashSet.insert initialThreadId
                    & mailboxes . at initialThreadId .~ Just Seq.empty
                    & (fst . nextThreadId)
                    & enqueue (suspendThread High initialThread)

-- | Run the threads that are scheduled in a 'SchedulerState' to completion.
loop :: Eq systemEvent => SchedulerState effs systemEvent -> Eff effs ()
loop s = do
    case dequeue s of
        AThread EmThread{_continuation, _threadId} event schedulerState | hasActiveUserThreads schedulerState -> do
            result <- _continuation event
            case result of
                Done () -> Trace.trace ("loop: Thread done: " <> show _threadId) $ loop $ schedulerState & removeActiveThread _threadId
                Continue WithPriority{_priority, _thread=sysCall} k -> do
                    let thisThread = suspendThread _priority EmThread{_threadId=_threadId, _continuation=k}
                        newState = schedulerState & enqueue thisThread & handleSysCall sysCall
                    loop newState
        _ -> Trace.trace "loop: Done" (pure ()) 

handleSysCall ::
    Eq systemEvent
    => SysCall effs systemEvent
    -> SchedulerState effs systemEvent
    -> SchedulerState effs systemEvent
handleSysCall sysCall schedulerState = case sysCall of
    Fork tp newThread ->
        let (schedulerState', tid) = nextThreadId schedulerState
        in Trace.trace "Fork" (
            enqueue (newThread tid) schedulerState'
                & activeThreads . at tp . non mempty %~ HashSet.insert tid
                & mailboxes . at tid .~ Just Seq.empty)
    Suspend -> schedulerState
    Broadcast msg -> Trace.trace "broadcast" (schedulerState & mailboxes . traversed %~ (|> msg))
    Message t msg -> Trace.trace "message" (schedulerState & mailboxes . at t . non mempty %~ (|> msg))

nextThreadId :: SchedulerState effs systemEvent -> (SchedulerState effs systemEvent, ThreadId)
nextThreadId s = (s & lastThreadId %~ ThreadId . succ . unThreadId, let i = s ^. lastThreadId in Trace.trace ("nextThreadId: " <> show i) i)

initialState :: SchedulerState effs systemEvent
initialState = SchedulerState Seq.empty Seq.empty Seq.empty initialThreadId HashMap.empty Map.empty

enqueue :: SuspendedThread effs systemEvent -> SchedulerState effs systemEvent -> SchedulerState effs systemEvent
enqueue WithPriority {_priority, _thread} s =
    case _priority of
        High     -> 
            let s' = s & highPrio %~ (|> _thread)
            in
                if numThreads s == numThreads s'
                    then (Trace.trace ("enqueue: High" <> desc s') s')
                    else s'
        Low      ->
            let s' = s & lowPrio %~ (|> _thread)
            in 
                if numThreads s == numThreads s'
                    then Trace.trace ("enqueue: Low" <> desc s') s'
                    else s'
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
    case s ^. highPrio of
        x :<| xs -> Trace.trace ("Dequeue: highPrio " <> desc s) (Just (s & highPrio .~ xs, x))
        Empty -> case s ^. lowPrio of
            x :<| xs -> Trace.trace ("Dequeue: lowPrio " <> desc s) $ Just (s & lowPrio .~ xs, x)
            Empty -> case s ^. sleeping of
                x :<| xs -> Just (s & sleeping .~ xs, x)
                Empty  -> Nothing

dequeueMessage :: SchedulerState effs systemEvent -> ThreadId -> Maybe (SchedulerState effs systemEvent, systemEvent)
dequeueMessage s i = do
    mailbox <- s ^. mailboxes . at i
    (x, xs) <- case mailbox of { Empty -> Nothing; x :<| xs -> Just (x, xs) }
    pure (s & mailboxes . at i .~ Just xs, x)
