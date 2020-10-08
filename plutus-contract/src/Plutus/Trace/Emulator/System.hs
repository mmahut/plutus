{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Emulator.System(
    launchSystemThreads
    ) where

import           Control.Monad                 (forever, forM_)
import           Control.Monad.Freer
import Data.Foldable (traverse_)
import           Control.Monad.Freer.Coroutine
import           Wallet.Emulator.Chain         (ChainControlEffect, ChainEffect, getCurrentSlot, processBlock)
import           Wallet.Emulator.MultiAgent                    (MultiAgentEffect, walletControlAction)

import           Plutus.Trace.Emulator.Types   (EmulatorEvent (..))
import           Plutus.Trace.Scheduler        (Priority (..), SysCall (..), SystemCall, fork, mkSysCall, sleep)
import           Wallet.Emulator.Wallet                        (Wallet(..))
import Wallet.Emulator.ChainIndex (chainIndexNotify)
import Wallet.Emulator.NodeClient (clientNotify, ChainClientNotification(..))

launchSystemThreads :: forall effs.
    ( Member ChainControlEffect effs
    , Member MultiAgentEffect effs
    , Member ChainEffect effs
    )
    => Eff (Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent) ': effs) ()
launchSystemThreads = do
    -- 1. Block maker
    _ <- fork @effs @EmulatorEvent Low (blockMaker @effs)
    -- 2. Threads for updating the agents' states
    traverse_ (fork @effs @EmulatorEvent Low . agentThread @effs) (Wallet <$> [1..10])

blockMaker :: forall effs effs2.
    ( Member ChainControlEffect effs2
    , Member ChainEffect effs2
    , Member (Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent)) effs2
    )
    => Eff effs2 ()
blockMaker = forever go where
    go = do
        newBlock <- processBlock
        _ <- mkSysCall @effs High (Broadcast $ BlockAdded newBlock)
        newSlot <- getCurrentSlot
        mkSysCall @effs Sleeping (Broadcast $ NewSlot newSlot)

agentThread :: forall effs effs2.
    ( Member MultiAgentEffect effs2
    , Member (Yield (SystemCall effs EmulatorEvent) (Maybe EmulatorEvent)) effs2
    )
    => Wallet
    -> Eff effs2 ()
agentThread wllt = forever go where
    go = do
        e <- sleep @effs @EmulatorEvent Sleeping
        let noti = e >>= \case
                BlockAdded block -> Just $ BlockValidated block
                NewSlot slot -> Just $ SlotChanged slot
                _ -> Nothing

        forM_ noti $ \n -> do
            walletControlAction wllt $ do
                clientNotify n
                chainIndexNotify n

