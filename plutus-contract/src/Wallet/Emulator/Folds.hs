{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{- This module provides a list of folds over the emulator event stream.
-}
module Wallet.Emulator.Folds (
    EmulatorEventFold
    , EmulatorFoldErr(..)
    -- * Folds for contract instances
    , instanceState
    , instanceRequests
    , instanceResponses
    , instanceOutcome
    , Outcome(..)
    -- * Folds for transactions and the UTXO set
    , failedTransactions
    , validatedTransactions
    , utxoAtAddress
    , valueAtAddress
    -- * Folds for individual wallets (emulated agents)
    , walletWatchingAddress
    , walletFunds
    -- * Etc.
    , preMapMaybeM
    , postMapM
    ) where

import Control.Foldl (FoldM(..), Fold(..))
import Control.Lens hiding (Empty, Fold)
import Control.Monad.Freer
import Control.Monad ((>=>))
import Data.Foldable (toList)
import Ledger.TxId (TxId)
import Control.Monad.Freer.Error
import Ledger.Tx (Address, Tx, TxOutTx(..), TxOut(..))
import Ledger.Index (ValidationError)
import Language.Plutus.Contract (Contract)
import qualified Control.Foldl as L
import Ledger.Value (Value)
import Data.Sequence (Seq(..))
import Plutus.Trace.Emulator.Types (ContractInstanceTag, ContractInstanceMsg, cilMessage, cilTag, _HandledRequest, ContractConstraints)
import           Language.Plutus.Contract.Schema               (Event (..), Handlers)
import Plutus.Trace.Emulator.ContractInstance (ContractInstanceState, addEventInstanceState, emptyInstanceState, instHandlersHistory, instEvents, instContractState)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types                              as JSON
import           Language.Plutus.Contract.Resumable (Response, Request)
import Wallet.Emulator.Chain (_TxnValidationFail, _TxnValidate)
import Language.Plutus.Contract.Types (ResumableResult(..))
import qualified Ledger.AddressMap as AM
import Ledger.AddressMap (UtxoMap)
import Wallet.Emulator.MultiAgent (EmulatorEvent, instanceEvent, eteEvent, chainEvent, chainIndexEvent)
import Wallet.Emulator.Wallet (Wallet, walletAddress)
import Wallet.Emulator.ChainIndex (_AddressStartWatching)

-- | A fold over emulator events that can fail with 'EmulatorFoldErr'
type EmulatorEventFold a = forall effs. Member (Error EmulatorFoldErr) effs => FoldM (Eff effs) EmulatorEvent a

-- | Transactions that failed to validate
failedTransactions ::
    EmulatorEventFold [(Tx, ValidationError)]
failedTransactions =
    preMapMaybeM (return . preview (eteEvent . chainEvent . _TxnValidationFail))
    $ L.generalize L.list

-- | Transactions that were validated
validatedTransactions ::
    EmulatorEventFold [Tx]
validatedTransactions =
    preMapMaybeM (return . preview (eteEvent . chainEvent . _TxnValidate))
    $ L.generalize L.list

-- | The state of a contract instance, recovered from the emulator log.
instanceState ::
    forall s e a.
    ContractConstraints s
    => Contract s e a
    -> ContractInstanceTag
    -> EmulatorEventFold (ContractInstanceState s e a)
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
    -> EmulatorEventFold [Request (Handlers s)]
instanceRequests con = fmap g . instanceState con where
    g s = case instHandlersHistory s of
            Empty -> []
            _ :|> x -> x

-- | The reponses received by the contract instance
instanceResponses :: 
    forall s e a.
    ContractConstraints s
    => Contract s e a
    -> ContractInstanceTag
    -> EmulatorEventFold [Response (Event s)]
instanceResponses con = fmap (toList . instEvents) . instanceState con

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
    -> EmulatorEventFold (Outcome e a)
instanceOutcome con =
    fmap (fromResumableResult . instContractState) . instanceState con

-- | Unspent outputs at an address
utxoAtAddress :: Monad m => Address -> FoldM m EmulatorEvent UtxoMap
utxoAtAddress addr =
    preMapMaybeM (return . preview (eteEvent . chainEvent . _TxnValidate))
    $ L.generalize
    $ Fold (flip AM.updateAddresses) (AM.addAddress addr mempty) (view (AM.fundsAt addr))

-- | The total value of unspent outputs at an address
valueAtAddress :: Monad m => Address -> FoldM m EmulatorEvent Value
valueAtAddress = fmap (foldMap (txOutValue . txOutTxOut)) . utxoAtAddress

-- | The funds belonging to a wallet
walletFunds :: Monad m => Wallet -> FoldM m EmulatorEvent Value
walletFunds = valueAtAddress . walletAddress

-- | Whether the wallet is watching an address
walletWatchingAddress :: Monad m => Wallet -> Address -> FoldM m EmulatorEvent Bool
walletWatchingAddress wllt addr = 
    preMapMaybeM (return . preview (eteEvent . chainIndexEvent wllt . _AddressStartWatching))
    $ L.generalize $ L.any ((==) addr)

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