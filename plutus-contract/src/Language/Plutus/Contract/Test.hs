{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Testing contracts with HUnit and Tasty
module Language.Plutus.Contract.Test(
      module X
    , TracePredicate
    , PredF(..)
    , Language.Plutus.Contract.Test.not
    , endpointAvailable
    , interestingAddress
    , queryingUtxoAt
    , assertDone
    , assertNotDone
    , assertContractError
    , assertOutcome
    , assertNoFailedTransactions
    , assertFailedTransaction
    , Outcome(..)
    , assertHooks
    , assertResponses
    , tx
    , anyTx
    , assertEvents
    , walletFundsChange
    , waitingForSlot
    , walletState
    , walletWatchingAddress
    , emulatorLog
    , fundsAtAddress
    -- * Checking predicates
    , checkPredicate
    , renderTraceContext
    ) where

import           Control.Lens                                    (at, view, (^.))
import           Control.Monad                                   (guard, unless)
import           Control.Monad.Freer                             (Eff)
import           Control.Monad.Freer.Log                         (LogMessage (..))
import           Control.Monad.Writer                            (MonadWriter (..), Writer, runWriter)
import qualified Data.Aeson                                      as JSON
import           Data.Bifunctor                                  (Bifunctor (..))
import           Data.Foldable                                   (fold, toList)
import           Data.Functor.Contravariant                      (Contravariant (..), Op (..))
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Maybe                                      (fromMaybe, mapMaybe)
import           Data.Proxy                                      (Proxy (..))
import           Data.Row                                        (AllUniqueLabels, Forall, HasType)
import           Data.Row.Internal                               (Unconstrained1)
import           Data.Semigroup                                  (First (..))
import           Data.Semigroup.Generic                          (GenericSemigroupMonoid (..))
import           Data.Sequence                                   (Seq)
import           Data.String                                     (IsString (..))
import qualified Data.Text                                       as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text           (renderStrict)
import           Data.Void
import           GHC.Generics                                    (Generic)
import           GHC.TypeLits                                    (KnownSymbol, Symbol, symbolVal)
import qualified Test.Tasty.HUnit                                as HUnit
import           Test.Tasty.Providers                            (TestTree)

import qualified Language.PlutusTx.Prelude                       as P

import           Language.Plutus.Contract.Checkpoint             (CheckpointStore)
import           Language.Plutus.Contract.Resumable              (IterationID, Request (..), RequestID, Response (..),
                                                                  Responses)
import qualified Language.Plutus.Contract.Resumable              as State
import           Language.Plutus.Contract.Types                  (Contract (..))
import qualified Language.Plutus.Contract.Types                  as Types
import           Language.PlutusTx.Lattice
import           Ledger.Constraints.OffChain                     (UnbalancedTx)

import           Language.Plutus.Contract.Effects.AwaitSlot      (SlotSymbol)
import qualified Language.Plutus.Contract.Effects.AwaitSlot      as AwaitSlot
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoints
import qualified Language.Plutus.Contract.Effects.UtxoAt         as UtxoAt
import qualified Language.Plutus.Contract.Effects.WatchAddress   as WatchAddress
import           Language.Plutus.Contract.Effects.WriteTx        (TxSymbol)
import qualified Language.Plutus.Contract.Effects.WriteTx        as WriteTx

import           Ledger.Address                                  (Address)
import qualified Ledger.AddressMap                               as AM
import           Ledger.Index                                    (ValidationError)
import           Ledger.Slot                                     (Slot)
import           Ledger.TxId                                     (TxId)
import           Ledger.Value                                    (Value)
import           Wallet.Emulator                                 (EmulatorEvent, EmulatorState (..))
import qualified Wallet.Emulator                                 as EM
import qualified Wallet.Emulator.Chain                           as EM
import           Wallet.Emulator.MultiAgent                      (EmulatorState, EmulatorTimeEvent (..))
import qualified Wallet.Emulator.MultiAgent                      as EM
import qualified Wallet.Emulator.NodeClient                      as EM

import           Language.Plutus.Contract.Schema                 (Event (..), Handlers (..), Input, Output)
import           Language.Plutus.Contract.Trace                  as X
import Plutus.Trace.Emulator (EmulatorConfig(..))
import           Plutus.Trace                                    (Emulator, Trace, runEmulatorTrace, defaultEmulatorConfig)
import           Plutus.Trace.Emulator                           (EmulatorErr)
import           Wallet.Types                                    (ContractInstanceId)

newtype PredF f a = PredF { unPredF :: a -> f Bool }
    deriving Contravariant via (Op (f Bool))

instance Applicative f => JoinSemiLattice (PredF f a) where
    l \/ r = PredF $ \a -> (\/) <$> unPredF l a <*> unPredF r a
instance Applicative f => MeetSemiLattice (PredF f a) where
    l /\ r = PredF $ \a -> (/\) <$> unPredF l a <*> unPredF r a

instance Applicative f => BoundedJoinSemiLattice (PredF f a) where
    bottom = PredF $ const (pure bottom)

instance Applicative f => BoundedMeetSemiLattice (PredF f a) where
    top = PredF $ const (pure top)

type TracePredicate = PredF (Writer (Doc Void)) (InitialDistribution, Maybe EmulatorErr, EmulatorState)

not :: TracePredicate -> TracePredicate
not = PredF . fmap (fmap Prelude.not) . unPredF

checkPredicate
    :: forall s e a
    . ( Show e
      , Forall (Input s) Pretty
      , Forall (Output s) Pretty
      , AllUniqueLabels (Input s)
      , Forall (Input s) JSON.FromJSON
      , Forall (Output s) Unconstrained1
      )
    => String
    -> TracePredicate
    -> Eff '[Trace Emulator] ()
    -> TestTree
checkPredicate nm predicate action = HUnit.testCaseSteps nm $ \step -> do
    let cfg = defaultEmulatorConfig
        (a, b) = runEmulatorTrace cfg action
        (result, testOutputs) = runWriter $ unPredF predicate (_initialDistribution cfg, either Just (const Nothing) a, b)
    unless result (step . Text.unpack $ renderTraceContext testOutputs b)
    HUnit.assertBool nm result

renderTraceContext
    :: forall ann.
    Doc ann
    -> EM.EmulatorState
    -> Text.Text
renderTraceContext testOutputs EmulatorState{_emulatorLog} =
    renderStrict $ layoutPretty defaultLayoutOptions $ vsep
        [ hang 2 (vsep ["Test outputs:", testOutputs])
        , hang 2 (vsep ("Emulator log:" : fmap pretty _emulatorLog))
        ]

endpointAvailable
    :: forall (l :: Symbol) s e a.
       ( HasType l Endpoints.ActiveEndpoint (Output s)
       , KnownSymbol l
       )
    => Wallet
    -> TracePredicate
endpointAvailable w = PredF $ \(_, r) ->
    if any (Endpoints.isActive @l @s) (hooks w r)
    then pure True
    else do
        tell ("missing endpoint:" <+> fromString (symbolVal (Proxy :: Proxy l)))
        pure False

interestingAddress
    :: forall s e a.
       ( WatchAddress.HasWatchAddress s )
    => Wallet
    -> Address
    -> TracePredicate
interestingAddress w addr = PredF $ \(_, r) -> do
    let hks = mapMaybe WatchAddress.watchedAddress (hooks w r)
    if any (== addr) hks
    then pure True
    else do
        tell $ hsep
            [ "Interesting addresses of " <+> pretty w <> colon
                <+> nest 2 (concatWith (surround (comma <> space))  (viaShow <$> toList hks))
            , "Missing address:", viaShow addr
            ]
        pure False

queryingUtxoAt
    :: forall s e a.
       ( UtxoAt.HasUtxoAt s )
    => Wallet
    -> Address
    -> TracePredicate
queryingUtxoAt w addr =  PredF $ \(_, r) -> do
    let hks = mapMaybe UtxoAt.utxoAtRequest (hooks w r)
    if any (== addr) hks
    then pure True
    else do
        tell $ hsep
            [ "UTXO queries of " <+> pretty w <> colon
                <+> nest 2 (concatWith (surround (comma <> space))  (viaShow <$> toList hks))
            , "Missing address:", viaShow addr
            ]
        pure False

tx
    :: forall s e a.
       ( HasType TxSymbol UnbalancedTx (Output s))
    => Wallet
    -> (UnbalancedTx -> Bool)
    -> String
    -> TracePredicate
tx w flt nm = PredF $ \(_, r) -> do
    let hks = mapMaybe WriteTx.pendingTransaction (hooks w r)
    if any flt hks
    then pure True
    else do
        tell $ hsep
            [ "Unbalanced transactions of" <+> pretty w <> colon
                <+> nest 2 (vsep (fmap pretty hks))
            , "No transaction with '" <> fromString nm <> "'"]
        pure False

walletState
    :: forall s e a.
       Wallet
    -> (EM.NodeClientState -> Bool)
    -> String
    -> TracePredicate
walletState w flt nm = PredF $ \(_, EmulatorState{_walletStates}) -> do
    case walletStates ^. at w of
        Nothing -> do
            tell $ "Wallet state of " <+> pretty w <+> "not found"
            pure False
        Just st ->
            if flt st
            then pure True
            else do
                tell $ vsep
                    [ "Wallet state of '" <+> viaShow w <+> colon <+> viaShow st
                    , "Fails " <> squotes (fromString nm)
                    ]
                pure False

walletWatchingAddress
    :: forall s e a.
       Wallet
    -> Address
    -> TracePredicate
walletWatchingAddress w addr =
    let desc = "watching address " <> show addr in
    walletState w (Map.member addr . AM.values . view EM.clientIndex) desc

assertEvents
    :: forall s e a.
       (Forall (Input s) Pretty)
    => Wallet
    -> ([Event s] -> Bool)
    -> String
    -> TracePredicate
assertEvents w pr nm = PredF $ \(_, r) -> do
    let es = fmap (toList . walletEvents) (view (ctsWalletStates . at w) $ _ctrTraceState r)
    case es of
        Nothing -> do
            tell $ "Event log for" <+> pretty w <+> "not found"
            pure False
        Just lg ->
            if pr (fmap State.rspResponse lg)
            then pure True
            else do
                tell $ vsep
                    [ "Event log for" <+> pretty w <> ":"
                    , nest 2 (vsep (fmap pretty lg))
                    , "Fails" <+> squotes (fromString nm)
                    ]
                pure False

-- | Check that the funds at an address meet some condition.
fundsAtAddress
    :: forall s e a.
       Address
    -> (Value -> Bool)
    -> TracePredicate
fundsAtAddress address check = PredF $ \(_, r) -> do
    let funds =
            Map.findWithDefault mempty address
            $ AM.values
            $ AM.fromChain
            $ view (EM.chainState . EM.chainNewestFirst)
            $ _ctrEmulatorState r
        passes = check funds
    unless passes
        $ tell ("Funds at address" <+> pretty address <+> "were" <> pretty funds)
    pure passes

waitingForSlot
    :: forall s e a.
       ( HasType SlotSymbol AwaitSlot.WaitingForSlot (Output s)
       )
    => Wallet
    -> Slot
    -> TracePredicate
waitingForSlot w sl = PredF $ \(_, r) ->
    case mapMaybe (\e -> AwaitSlot.request e >>= guard . (==) sl) (hooks w r) of
        [] -> do
            tell $ pretty w <+> "not waiting for any slot notifications. Expected:" <+>  viaShow sl
            pure False
        _ -> pure True

emulatorLog
    :: forall s e a.
       ()
    => ([LogMessage EmulatorEvent] -> Bool)
    -> String
    -> TracePredicate
emulatorLog f nm = PredF $ \(_, r) ->
    let lg = EM._emulatorLog $ _ctrEmulatorState r in
    if f lg
    then pure True
    else do
        tell $ vsep
            [ "Emulator log:"
            , nest 2 (vsep (fmap pretty lg))
            , "Fails" <+> squotes (fromString nm)
            ]
        pure False

anyTx
    :: forall s e a.
       ( HasType TxSymbol UnbalancedTx (Output s)
       )
    => Wallet
    -> TracePredicate
anyTx w = tx w (const True) "anyTx"

assertHooks
    :: forall s e a.
       ( Forall (Output s) Pretty
       )
    => Wallet
    -> ([Handlers s] -> Bool)
    -> String
    -> TracePredicate
assertHooks w p nm = PredF $ \(_, rs) ->
    let hks = hooks w rs in
    if p hks
    then pure True
    else do
        tell $ vsep
            [ "Handlers for" <+> pretty w <> colon
            , nest 2 (pretty hks)
            , "Failed" <+> squotes (fromString nm)
            ]
        pure False

-- | Make an assertion about the responses provided to the contract instance.
assertResponses
    :: forall s e a.
       ( Forall (Input s) Pretty
       )
    => Wallet
    -> (Responses (Event s) -> Bool)
    -> String
    -> TracePredicate
assertResponses w p nm = PredF $ \(_, rs) ->
    case record w rs of
        r
            | p r -> pure True
            | otherwise -> do
                tell $ vsep
                    [ "Record:"
                    , nest 2 (pretty r)
                    , "Failed" <+> squotes (fromString nm)
                    ]
                pure False

data Outcome e a =
    Done a
    -- ^ The contract finished without errors and produced a result
    | NotDone
    -- ^ The contract is waiting for more input.
    | Error e
    -- ^ The contract failed with an error.
    deriving (Eq, Show)

-- | A 'TracePredicate' checking that the wallet's contract instance finished
--   without errors.
assertDone
    :: forall s e a.
    ( Forall (Input s) Pretty
    , Forall (Output s) Pretty
    , Show e
    )
    => Wallet
    -> (a -> Bool)
    -> String
    -> TracePredicate
assertDone w pr = assertOutcome w (\case { Done a -> pr a; _ -> False})

-- | A 'TracePredicate' checking that the wallet's contract instance is
--   waiting for input.
assertNotDone
    :: forall s e a.
    ( Forall (Input s) Pretty
    , Forall (Output s) Pretty
    , Show e
    )
    => Wallet
    -> String
    -> TracePredicate
assertNotDone w = assertOutcome w (\case { NotDone -> True; _ -> False})

-- | A 'TracePredicate' checking that the wallet's contract instance
--   failed with an error.
assertContractError
    :: forall s e a.
    ( Forall (Input s) Pretty
    , Forall (Output s) Pretty
    , Show e
    )
    => Wallet
    -> (e -> Bool)
    -> String
    -> TracePredicate
assertContractError w p =
    assertOutcome w (\case { Error err -> p err; _ -> False })

assertOutcome
    :: forall s e a.
       ( Forall (Input s) Pretty
       , Forall (Output s) Pretty
       , Show e
       )
    => Wallet
    -> (Outcome e a -> Bool)
    -> String
    -> TracePredicate
assertOutcome w p nm = PredF $ \(_, rs) ->
    let (evts, con) = contractEventsWallet rs w
        store       = contractCheckpointStore w rs
        result = Types.runResumable evts store (unContract con)
    in
        case result of
            Types.ResumableResult{Types.wcsFinalState=Left err, Types.wcsRequests, Types.wcsLogs}
                | p (Error err) -> pure True
                | otherwise -> do
                    tell $ vsep
                        [ "Outcome of" <+> pretty w <> colon
                        , "Failed with error" <+> viaShow err
                        , pretty wcsRequests
                        , prettyLogs wcsLogs
                        , "in" <+> squotes (fromString nm)
                        ]
                    pure False
            Types.ResumableResult{Types.wcsFinalState=Right Nothing,Types.wcsRequests, Types.wcsLogs}
                | p NotDone -> pure True
                | otherwise -> do
                    tell $ vsep
                        [ "Outcome of" <+> pretty w <> colon
                        , "Open record"
                        , pretty wcsRequests
                        , prettyLogs wcsLogs
                        , "in" <+> squotes (fromString nm)
                        ]
                    pure False
            Types.ResumableResult{Types.wcsFinalState=Right (Just a),Types.wcsResponses, Types.wcsLogs}
                | p (Done a) -> pure True
                | otherwise -> do
                    tell $ vsep
                        [ "Outcome of" <+> pretty w <> colon
                        , "Closed record"
                        , pretty wcsResponses
                        , prettyLogs wcsLogs
                        , "failed with" <+> squotes (fromString nm)
                        ]
                    pure False

contractInstancesByWallet
    :: EmulatorState
    -> Map Wallet ContractInstanceId
contractInstancesByWallet = Map.fromList . mapMaybe f . view emulatorLog where
    f :: LogMessage EmulatorEvent -> Maybe (Wallet, ContractInstanceId)
    f = preview (logMessageContent . instanceEvent . _)

-- | Events that were received by the contract instance
contractInstanceEvents ::
    forall s.  ()
    => EmulatorState
    -> ContractInstanceId
    -> [Response (Event s)]
contractInstanceEvents EmulatorState{_emulatorLog} w =
    -- let p = logMessageContent . instanceEvent .
    --     evts = Map.findWithDefault [] w (eventsByWallet $ _ctrTraceState rs)
    --     con  = rs ^. ctrTraceState . ctsContract
    -- in (evts, con)
    undefined -- FIXME

contractCheckpointStore
    :: ContractInstanceId
    -> EmulatorState
    -> CheckpointStore
contractCheckpointStore i w = undefined
    -- fromMaybe mempty . view (at w) . checkpointStoreByWallet . _ctrTraceState

walletFundsChange
    :: forall s e a.
       ()
    => Wallet
    -> Value
    -> TracePredicate
walletFundsChange w dlt = PredF $ \(initialDist, st) ->
        let initialValue = fold (initialDist ^. at w)
            finalValue   = fromMaybe mempty (EM.fundsDistribution st ^. at w)
        in if initialValue P.+ dlt == finalValue
        then pure True
        else do
            tell $ vsep
                [ "Expected funds of" <+> pretty w <+> "to change by" <+> viaShow dlt
                , "but they changed by", viaShow (finalValue P.- initialValue)]
            pure False

-- | Assert that at least one transaction failed to validated, and that all
--   transactions that failed meet the predicate.
assertFailedTransaction
    :: forall s e a
    .  (TxId -> ValidationError -> Bool)
    -> TracePredicate
assertFailedTransaction predicate = PredF $ \(_, st) ->
    case failedTransactions (EM.emLog st) of
        [] -> do
            tell $ "No transactions failed to validate."
            pure False
        xs -> pure (all (uncurry predicate) xs)

-- | Assert that no transaction failed to validate.
assertNoFailedTransactions
    :: forall s e a.
    TracePredicate
assertNoFailedTransactions = PredF $ \(_, st) ->
    case failedTransactions (EM.emLog st) of
        [] -> pure True
        xs -> do
            tell $ vsep ("Transactions failed to validate:" : fmap pretty xs)
            pure False

failedTransactions :: [LogMessage EmulatorEvent] -> [(TxId, ValidationError)]
failedTransactions = mapMaybe $
    \case
        LogMessage{_logMessageContent=EmulatorTimeEvent{_eteEvent=EM.ChainEvent (EM.TxnValidationFail txid err)}} -> Just (txid, err)
        _ -> Nothing

prettyLogs :: Seq (LogMessage JSON.Value) -> Doc ann
prettyLogs theLogs =
    let theLines = vsep . fmap (pretty . fmap show) $ toList theLogs
    in hang 2 $ vsep ["Logs:", theLines]
