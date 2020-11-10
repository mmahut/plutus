{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Running emulator actions that produce streams of events
module Wallet.Emulator.Stream(
    -- * Emulator streams
    EmulatorConfig(..)
    , EmulatorErr(..)
    , initialDistribution
    , defaultEmulatorConfig
    , runTraceStream
    -- * Stream manipulation
    , takeUntilSlot
    , filterLogLevel
    -- * Consuming streams
    , foldStreamM
    , foldEmulatorStreamM
    ) where

import Control.Lens (preview, view, makeLenses, filtered)
import Control.Monad.Freer (Eff, run, interpret, Member, type (~>), reinterpret, subsume)
import           Control.Monad.Freer.Coroutine                   (Yield, yield)
import Control.Monad.Freer.State (State, evalState, gets)
import Control.Monad.Freer.Error (Error, runError)
import           Control.Monad.Freer.Extras                      (raiseEnd6, wrapError)
import Control.Monad.Freer.Stream (runStream)
import qualified Control.Foldl as L
import           Control.Monad.Freer.Log                         (LogMessage, logMessageContent, LogMsg(..), mapMLog, LogLevel, LogMessage(..))
import qualified Data.Map                                        as Map
import Ledger.Slot (Slot)
import           Wallet.Emulator                                 (EmulatorEvent, EmulatorEvent')
import           Wallet.API                                      (WalletAPIError)
import Wallet.Emulator.MultiAgent (eteEvent, chainEvent, EmulatorState, MultiAgentEffect, EmulatorTimeEvent(..))
import Wallet.Emulator.Chain (_SlotAdd, ChainEffect, ChainControlEffect)
import qualified Wallet.Emulator                                 as EM
import qualified Streaming.Prelude as S
import Streaming (Stream)
import Streaming.Prelude (Of)
import qualified Streaming as S

-- TODO: Move these two to 'Wallet.Emulator.XXX'?
import           Language.Plutus.Contract.Trace                  (InitialDistribution, defaultDist)
import           Plutus.Trace.Emulator.ContractInstance          (ContractInstanceError)

-- | Finish the stream at the end of the given slot.
takeUntilSlot :: forall effs a. Slot -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) ()
takeUntilSlot maxSlot = S.takeWhile (maybe True (\sl -> sl <= maxSlot) . preview (logMessageContent . eteEvent . chainEvent . _SlotAdd))

-- | Remove from the stream all log messages whose log level is lower than the
--   the given level.
filterLogLevel :: forall effs a. LogLevel -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a
filterLogLevel lvl = S.mapMaybe (preview (filtered (\LogMessage{_logLevel} -> lvl <= _logLevel)))

-- | Apply a fold to an effectful stream of events.
foldStreamM :: forall m a b c.
    Monad m
    => L.FoldM m a b
    -> S.Stream (S.Of a) m c
    -> m (S.Of b c)
foldStreamM theFold = L.impurely S.foldM theFold

-- | Consume an emulator event stream. Make sure that the stream terminates
--   (either with 'takeUntilSlot', or by using a short-circuiting effect
--   such as 'Error')
foldEmulatorStreamM :: forall effs a b.
    L.FoldM (Eff effs) EmulatorEvent b
    -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a
    -> Eff effs (S.Of b a)
foldEmulatorStreamM theFold =
    foldStreamM (L.premapM (pure . view logMessageContent) theFold)

-- | Turn an emulator action into a potentially infinite 'Stream' of emulator
--   log messages.
runTraceStream :: forall effs.
    EmulatorConfig
    -> Eff '[ State EmulatorState
            , LogMsg EmulatorEvent'
            , MultiAgentEffect
            , ChainEffect
            , ChainControlEffect
            , Error ContractInstanceError
            ] ()
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr)
runTraceStream conf =  
    fmap (either Just (const Nothing))
    . S.hoist (pure . run)
    . runStream @(LogMessage EmulatorEvent) @_ @'[]
    . evalState (initialState conf)
    . interpret handleLogCoroutine
    . reinterpret @_ @(LogMsg EmulatorEvent) (mkTimedLogs @EmulatorEvent')
    . runError
    . wrapError WalletErr
    . wrapError AssertionErr
    . wrapError InstanceErr
    . EM.processEmulated
    . subsume
    . subsume @(State EmulatorState)
    . raiseEnd6

newtype EmulatorConfig =
    EmulatorConfig
        { _initialDistribution :: InitialDistribution 
        }

defaultEmulatorConfig :: EmulatorConfig
defaultEmulatorConfig =
    EmulatorConfig
        { _initialDistribution = defaultDist
        }

initialState :: EmulatorConfig -> EM.EmulatorState
initialState EmulatorConfig{_initialDistribution} = EM.emulatorStateInitialDist (Map.mapKeys EM.walletPubKey _initialDistribution)

data EmulatorErr =
    WalletErr WalletAPIError
    | AssertionErr EM.AssertionError
    | InstanceErr ContractInstanceError
    deriving (Show)

handleLogCoroutine :: forall e effs.
    Member (Yield (LogMessage e) ()) effs
    => LogMsg e
    ~> Eff effs
handleLogCoroutine = \case LMessage m -> yield m id

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

makeLenses ''EmulatorConfig