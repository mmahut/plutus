{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
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
    , Tag
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
    , SchedulerLog(..)
    , ThreadEvent(..)
    ) where


import           Control.Lens                     hiding (Empty)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Log          (LogMsg, logDebug, logInfo)
import           Control.Monad.Freer.Reader
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Hashable                    (Hashable)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.HashSet                     (HashSet)
import qualified Data.HashSet                     as HashSet
import           Data.Map                         as Map
import           Data.Sequence                    (Seq (..))
import qualified Data.Sequence                    as Seq
import Data.Text (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Extras (PrettyShow (..), Tagged (..))
import           GHC.Generics                     (Generic)
import Plutus.Trace.Tag (Tag)

newtype ThreadId = ThreadId { unThreadId :: Int }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, ToJSON, FromJSON)
    deriving Pretty via (Tagged "Thread" Int)

initialThreadId :: ThreadId
initialThreadId = ThreadId 0

data Priority = Low | High | Sleeping
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving Pretty via (PrettyShow Priority)

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
        , _tag          :: Tag
        }

-- | The "system calls" we can make when a 'Simulator' action.
data SysCall effs systemEvent
    = Fork (ThreadId -> SuspendedThread effs systemEvent)
    | Suspend
    | Broadcast systemEvent
    | Message ThreadId systemEvent

makePrisms ''SysCall

-- | Scheduler state consisting of three queues of suspended threads, one for
--   each 'Priority' level.
data SchedulerState effs systemEvent
    = SchedulerState
        { _highPrio      :: Seq (EmThread effs systemEvent)
        , _lowPrio       :: Seq (EmThread effs systemEvent)
        , _sleeping      :: Seq (EmThread effs systemEvent)
        , _lastThreadId  :: ThreadId
        , _mailboxes     :: HashMap ThreadId (Seq systemEvent)
        , _activeThreads :: Map Tag (HashSet ThreadId)
        }

makeLenses ''SchedulerState

removeActiveThread :: ThreadId -> SchedulerState effs systemEvent -> SchedulerState effs systemEvent
removeActiveThread tid = over (activeThreads . mapped) (HashSet.delete tid)

threadsByTag :: Tag -> SchedulerState effs systemEvent -> HashSet ThreadId
threadsByTag tag = view (activeThreads . at tag . non mempty)

suspendThread :: Priority -> EmThread effs systemEvent -> SuspendedThread effs systemEvent
suspendThread = WithPriority

-- | Make a thread with the given priority from an action. This is a
--   convenience for defining 'SimulatorInterpreter' values.
mkThread :: Tag -> Priority -> Eff (Reader ThreadId ': Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) () -> ThreadId -> SuspendedThread effs systemEvent
mkThread tag prio action tid =
    let action' = runReader tid action
    in WithPriority
            { _priority = prio
            , _thread = EmThread
                { _threadId = tid
                , _continuation = \_ -> runC action'
                , _tag      = tag
                }
            }

mkSysCall :: forall effs systemEvent effs2.
    Member (Yield (SystemCall effs systemEvent) (Maybe systemEvent)) effs2
    => Priority
    -> SysCall effs systemEvent
    -> Eff effs2 (Maybe systemEvent)
mkSysCall prio sc = yield @(SystemCall effs systemEvent) @(Maybe systemEvent) (WithPriority prio sc) id

-- | Start a new thread with the given priority
fork :: forall effs systemEvent effs2.
    Member (Yield (SystemCall effs systemEvent) (Maybe systemEvent)) effs2
    => Tag
    -> Priority
    -> Eff (Reader ThreadId ': Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) ()
    -> Eff effs2 (Maybe systemEvent)
fork tag prio action = mkSysCall prio (Fork $ mkThread tag prio action)

sleep :: forall effs systemEvent effs2.
    Member (Yield (SystemCall effs systemEvent) (Maybe systemEvent)) effs2
    => Priority
    -> Eff effs2 (Maybe systemEvent)
sleep prio = mkSysCall @effs @systemEvent @effs2 prio Suspend

initialThreadTag :: Tag
initialThreadTag = "initial thread"

runThreads ::
    forall effs systemEvent.
    ( Eq systemEvent
    , Member (LogMsg SchedulerLog) effs
    )
    => Eff (Yield (SystemCall effs systemEvent) (Maybe systemEvent) ': effs) ()
    -> Eff effs ()
runThreads e = do
    k <- runC e
    case k of
        Done () -> pure ()
        Continue _ k' ->
            let initialThread = EmThread{_continuation = k', _threadId = initialThreadId, _tag = initialThreadTag}
            in loop
                $ initialState
                    & activeThreads . at initialThreadTag . non mempty %~ HashSet.insert initialThreadId
                    & mailboxes . at initialThreadId .~ Just Seq.empty
                    & (fst . nextThreadId)
                    & enqueue (suspendThread High initialThread)

-- | Run the threads that are scheduled in a 'SchedulerState' to completion.
loop :: forall effs systemEvent.
    ( Eq systemEvent
    , Member (LogMsg SchedulerLog) effs
    )
    => SchedulerState effs systemEvent
    -> Eff effs ()
loop s = do
    case dequeue s of
        AThread EmThread{_continuation, _threadId, _tag} event schedulerState prio -> do
            let mkLog e = SchedulerLog{slEvent=e, slThread=_threadId, slPrio=prio, slTag = _tag}
            logDebug (mkLog Resumed)
            result <- _continuation event
            case result of
                Done () -> do
                    logInfo (mkLog Stopped)
                    loop $ schedulerState & removeActiveThread _threadId
                Continue WithPriority{_priority, _thread=sysCall} k -> do
                    logDebug SchedulerLog{slEvent=Suspended, slThread=_threadId, slPrio=_priority, slTag = _tag}
                    let thisThread = suspendThread _priority EmThread{_threadId=_threadId, _continuation=k, _tag = _tag}
                    newState <- schedulerState & enqueue thisThread & handleSysCall sysCall
                    loop newState
        _ -> pure ()

handleSysCall ::
    ( Eq systemEvent
    , Member (LogMsg SchedulerLog) effs
    )
    => SysCall effs systemEvent
    -> SchedulerState effs systemEvent
    -> Eff effs (SchedulerState effs systemEvent)
handleSysCall sysCall schedulerState = case sysCall of
    Fork newThread -> do
        let (schedulerState', tid) = nextThreadId schedulerState
            t = newThread tid
            tag = _tag $ _thread t
            newState = enqueue t schedulerState'
                        & activeThreads . at tag . non mempty %~ HashSet.insert tid
                        & mailboxes . at tid .~ Just Seq.empty
        logInfo $ SchedulerLog{slEvent = Started, slThread = tid, slPrio = _priority t, slTag = tag}
        pure newState
    Suspend -> pure schedulerState
    Broadcast msg -> pure $ schedulerState & mailboxes . traversed %~ (|> msg)
    Message t msg -> pure $ schedulerState & mailboxes . at t . non mempty %~ (|> msg)

nextThreadId :: SchedulerState effs systemEvent -> (SchedulerState effs systemEvent, ThreadId)
nextThreadId s = (s & lastThreadId %~ ThreadId . succ . unThreadId, s ^. lastThreadId)

initialState :: SchedulerState effs systemEvent
initialState = SchedulerState Seq.empty Seq.empty Seq.empty initialThreadId HashMap.empty Map.empty

enqueue :: SuspendedThread effs systemEvent -> SchedulerState effs systemEvent -> SchedulerState effs systemEvent
enqueue WithPriority {_priority, _thread} s =
    case _priority of
        High     -> s & highPrio %~ (|> _thread)
        Low      ->s & lowPrio %~ (|> _thread)
        Sleeping -> s & sleeping %~ (|> _thread)

-- | Result of calling 'dequeue'. Either a thread that is ready to receive a message,
--   or no more threads.
data SchedulerDQResult effs systemEvent
    = AThread (EmThread effs systemEvent) (Maybe systemEvent) (SchedulerState effs systemEvent) Priority
    | NoMoreThreads

dequeue :: SchedulerState effs systemEvent -> SchedulerDQResult effs systemEvent
dequeue s = case dequeueThread s of
    Nothing -> NoMoreThreads
    Just (s', thread, prio) -> case dequeueMessage s' (_threadId thread) of
        Nothing       -> AThread thread Nothing s' prio
        Just (s'', m) -> AThread thread (Just m) s'' prio

dequeueThread :: SchedulerState effs systemEvent -> Maybe (SchedulerState effs systemEvent, EmThread effs systemEvent, Priority)
dequeueThread s =
    case s ^. highPrio of
        x :<| xs -> Just (s & highPrio .~ xs, x, High)
        Empty -> case s ^. lowPrio of
            x :<| xs -> Just (s & lowPrio .~ xs, x, Low)
            Empty -> case s ^. sleeping of
                x :<| xs -> Just (s & sleeping .~ xs, x, Sleeping)
                Empty    -> Nothing

dequeueMessage :: SchedulerState effs systemEvent -> ThreadId -> Maybe (SchedulerState effs systemEvent, systemEvent)
dequeueMessage s i = do
    mailbox <- s ^. mailboxes . at i
    (x, xs) <- case mailbox of { Empty -> Nothing; x :<| xs -> Just (x, xs) }
    pure (s & mailboxes . at i .~ Just xs, x)

---
--- Logging etc.
---

data ThreadEvent = Stopped | Resumed | Suspended | Started
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving Pretty via (PrettyShow ThreadEvent)

data SchedulerLog =
    SchedulerLog
        { slEvent  :: ThreadEvent
        , slThread :: ThreadId
        , slTag    :: Tag
        , slPrio   :: Priority
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty SchedulerLog where
    pretty SchedulerLog{slEvent, slThread, slTag, slPrio} =
        pretty slThread <+> pretty slTag <> colon <+> pretty slEvent <+> parens (pretty slPrio)
