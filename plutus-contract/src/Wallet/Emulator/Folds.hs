{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{- This module provides a list of folds over the emulator event stream.
-}
module Wallet.Emulator.Folds (
    EmulatorEventFold
    , EmulatorEventFoldM
    , EmulatorFoldErr(..)
    -- * Folds for contract instances
    , instanceState
    , instanceRequests
    , instanceResponses
    , instanceOutcome
    , Outcome(..)
    , instanceLog
    -- * Folds for transactions and the UTXO set
    , failedTransactions
    , validatedTransactions
    , utxoAtAddress
    , valueAtAddress
    -- * Folds for individual wallets (emulated agents)
    , walletWatchingAddress
    , walletFunds
    -- * Annotated blockchain (used in Playground)
    , annotatedBlockchain
    -- * Etc.
    , preMapMaybeM
    , preMapMaybe
    , postMapM
    ) where

import Control.Foldl (FoldM(..), Fold(..))
import Control.Lens hiding (Empty, Fold)
import Control.Monad.Freer
import Control.Monad ((>=>))
import Data.Foldable (toList)
import Control.Monad.Freer.Error
import Ledger.Tx (Address, Tx, TxOutTx(..), TxOut(..))
import Ledger.Index (ValidationError)
import Language.Plutus.Contract (Contract)
import qualified Control.Foldl as L
import Ledger.Value (Value)
import Plutus.Trace.Emulator.Types (ContractInstanceTag, cilMessage, cilTag, _HandledRequest, ContractConstraints, ContractInstanceLog)
import           Language.Plutus.Contract.Schema               (Event (..), Handlers)
import Plutus.Trace.Emulator.ContractInstance (ContractInstanceState, addEventInstanceState, emptyInstanceState, instEvents, instContractState)
import qualified Data.Aeson as JSON
import           Language.Plutus.Contract.Resumable (Response, Request)
import Wallet.Emulator.Chain (_TxnValidationFail, _TxnValidate)
import qualified Language.Plutus.Contract.Resumable            as State
import Language.Plutus.Contract.Types (ResumableResult(..))
import qualified Ledger.AddressMap as AM
import Ledger.AddressMap (UtxoMap)
import Wallet.Emulator.MultiAgent (EmulatorEvent, instanceEvent, eteEvent, chainEvent, chainIndexEvent)
import Wallet.Emulator.Wallet (Wallet, walletAddress)
import Wallet.Emulator.ChainIndex (_AddressStartWatching)
import Wallet.Rollup.Types (AnnotatedTx)
import qualified Wallet.Rollup as Rollup

type EmulatorEventFold a = Fold EmulatorEvent a

-- | A fold over emulator events that can fail with 'EmulatorFoldErr'
type EmulatorEventFoldM a = forall effs. Member (Error EmulatorFoldErr) effs => FoldM (Eff effs) EmulatorEvent a

-- | Transactions that failed to validate
failedTransactions :: EmulatorEventFold [(Tx, ValidationError)]
failedTransactions = preMapMaybe (preview (eteEvent . chainEvent . _TxnValidationFail)) L.list

-- | Transactions that were validated
validatedTransactions :: EmulatorEventFold [Tx]
validatedTransactions = preMapMaybe (preview (eteEvent . chainEvent . _TxnValidate)) L.list

-- | The state of a contract instance, recovered from the emulator log.
instanceState ::
    forall s e a.
    ContractConstraints s
    => Contract s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM (ContractInstanceState s e a)
instanceState con tag = 
    let flt :: EmulatorEvent -> Maybe (Response JSON.Value)
        flt = preview (eteEvent . instanceEvent . filtered ((==) tag . view cilTag) . cilMessage . _HandledRequest)
        decode :: forall effs. Member (Error EmulatorFoldErr) effs => EmulatorEvent -> Eff effs (Maybe (Response (Event s)))
        decode e = do
            case flt e of
                Nothing -> pure Nothing
                Just response -> case traverse (JSON.fromJSON @(Event s)) response of
                    JSON.Error e' -> throwError $ JSONDecodingError e' response
                    JSON.Success e' -> pure (Just e')

    in preMapMaybeM decode $ L.generalize $ Fold (flip addEventInstanceState) (emptyInstanceState con) id

-- | The list of open requests of the contract instance at its latest iteration
instanceRequests ::
    forall s e a.
    ContractConstraints s
    => Contract s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM [Request (Handlers s)]
instanceRequests con = fmap g . instanceState con where
    g = State.unRequests . wcsRequests . instContractState

-- | The reponses received by the contract instance
instanceResponses :: 
    forall s e a.
    ContractConstraints s
    => Contract s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM [Response (Event s)]
instanceResponses con = fmap (toList . instEvents) . instanceState con

-- | The log messages produced by the contract instance.
instanceLog :: ContractInstanceTag -> EmulatorEventFold [ContractInstanceLog]
instanceLog tag = 
    let flt :: EmulatorEvent -> Maybe ContractInstanceLog
        flt = preview (eteEvent . instanceEvent . filtered ((==) tag . view cilTag))
    in preMapMaybe flt $ Fold (flip (:)) [] reverse

data Outcome e a =
    Done a
    -- ^ The contract finished without errors and produced a result
    | NotDone
    -- ^ The contract is waiting for more input.
    | Failed e
    -- ^ The contract failed with an error.
    deriving (Eq, Show)

fromResumableResult :: ResumableResult e i o a -> Outcome e a
fromResumableResult = either Failed (maybe NotDone Done) . wcsFinalState

-- | The final state of the instance
instanceOutcome :: 
    forall s e a.
    ContractConstraints s
    => Contract s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM (Outcome e a)
instanceOutcome con =
    fmap (fromResumableResult . instContractState) . instanceState con

-- | Unspent outputs at an address
utxoAtAddress :: Address -> EmulatorEventFold UtxoMap
utxoAtAddress addr =
    preMapMaybe (preview (eteEvent . chainEvent . _TxnValidate))
    $ Fold (flip AM.updateAddresses) (AM.addAddress addr mempty) (view (AM.fundsAt addr))

-- | The total value of unspent outputs at an address
valueAtAddress :: Address -> EmulatorEventFold Value
valueAtAddress = fmap (foldMap (txOutValue . txOutTxOut)) . utxoAtAddress

-- | The funds belonging to a wallet
walletFunds :: Wallet -> EmulatorEventFold Value
walletFunds = valueAtAddress . walletAddress

-- | Whether the wallet is watching an address
walletWatchingAddress :: Wallet -> Address -> EmulatorEventFold Bool
walletWatchingAddress wllt addr = 
    preMapMaybe (preview (eteEvent . chainIndexEvent wllt . _AddressStartWatching))
    $ L.any ((==) addr)

-- | Annotate the transactions that were validated by the node
annotatedBlockchain :: EmulatorEventFold [[AnnotatedTx]]
annotatedBlockchain = 
    preMapMaybe (preview (eteEvent . chainEvent))
    $ Fold Rollup.handleChainEvent Rollup.initialState Rollup.getAnnotatedTransactions

-- | An effectful 'Data.Maybe.mapMaybe' for 'FoldM'.
preMapMaybeM ::
    Monad m
    => (a -> m (Maybe b))
    -> FoldM m b r
    -> FoldM m a r
preMapMaybeM f (FoldM step begin done) = FoldM step' begin done where
    step' x a = do
        result <- f a
        case result of
            Nothing -> pure x
            Just a' -> step x a'

-- | 'Data.Maybe.mapMaybe' for 'Fold'.
preMapMaybe :: (a -> Maybe b) -> Fold b r -> Fold a r
preMapMaybe f (Fold step begin done) = Fold step' begin done where
    step' x a = case f a of
        Nothing -> x
        Just b -> step x b

-- | Effectfully map the result of a 'FoldM'
postMapM ::
    Monad m
    => (b -> m c)
    -> FoldM m a b
    -> FoldM m a c
postMapM f (FoldM step begin done) = FoldM step begin (done >=> f)

data EmulatorFoldErr =
    JSONDecodingError String (Response JSON.Value)
    deriving stock (Eq, Ord, Show)