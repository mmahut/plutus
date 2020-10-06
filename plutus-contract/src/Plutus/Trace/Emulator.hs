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
    , activateContract
    , callEndpoint
    , payToWallet
    , waitUntilSlot
    , etrace
    ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Reader     (Reader, ask, runReader)
import           Control.Monad.Freer.State      (State, modify, evalState)
import           Control.Monad.Freer.TH         (makeEffect)
import qualified Data.Aeson                     as JSON
import           Data.Map                       (Map)
import           Data.Proxy                     (Proxy (..))
import           Data.Void                      (Void)
import           Language.Plutus.Contract       (type (.\/), BlockchainActions, Contract, Endpoint, HasEndpoint)
import           Language.Plutus.Contract.Trace (handleBlockchainQueries)
import           Ledger.Slot                    (Slot)
import           Language.Plutus.Contract                          (Contract (..), HasAwaitSlot, HasTxConfirmation,
                                                                    HasUtxoAt, HasWatchAddress, HasWriteTx, mapError)
import           Language.Plutus.Contract.Resumable                (Request (..), Requests (..), Response (..))
import qualified Language.Plutus.Contract.Types                    as Contract.Types
import           Ledger.Value                   (Value)
import qualified Language.Plutus.Contract.Resumable                as State
import           Plutus.Trace.Scheduler         (Priority (..), SystemCall (..), SuspendedThread, SysCall (..),
                                                 ThreadId, mkSysCall, mkThread)
import           Wallet.Effects
import           Wallet.Emulator.Wallet         (Wallet (..))
import           Wallet.Types                   (ContractInstanceId)
import           Language.Plutus.Contract.Types                    (ResumableResult (..))
import           Language.Plutus.Contract.Schema                   (Event (..), Handlers (..), Input, Output)
import Data.Sequence (Seq)

import           Plutus.Trace.Types
import Plutus.Trace.Emulator.ContractInstance ()
import Plutus.Trace.Emulator.Types ()

data ContractInstanceIdEff r where
    UniqueId :: ContractInstanceIdEff ContractInstanceId
makeEffect ''ContractInstanceIdEff

i :: forall effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    )
    => SimulatorInterpreter Emulator effs EmulatorEvent
i = SimulatorInterpreter
    { _runLocal = emRunLocal
    , _runGlobal = emRunGlobal
    }

emRunLocal :: forall b effs.
    ( Member ContractInstanceIdEff effs
    , Member (State EmulatorState) effs
    )
    => Wallet
    -> EmulatorLocal b
    -> Eff effs (b, ThreadId -> SuspendedThread effs EmulatorEvent)
emRunLocal wllt = \case
    ActivateContract con -> do
        i <- uniqueId
        let handle = ContractHandle{chContract=con, chInstanceId = i}
        pure (handle, mkThread High (runReader wllt $ contractThread handle))

emRunGlobal :: forall b effs. EmulatorGlobal b -> Eff effs (b, ThreadId -> SuspendedThread effs EmulatorEvent)
emRunGlobal = undefined

