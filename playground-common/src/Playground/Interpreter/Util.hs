{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Playground.Interpreter.Util
    ( stage
    , renderInstanceTrace
    ) where

import Control.Monad.Freer (Eff, run)
import qualified Control.Foldl as L
import           Control.Monad.Freer.Error                       (Error, runError)
import Control.Monad (void)
import           Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions, vsep, pretty)
import           Data.Text.Prettyprint.Doc.Render.Text           (renderStrict)
import Streaming.Prelude (fst')
import           Data.Aeson                                      (FromJSON, eitherDecode)
import           Data.Bifunctor                                  (first)
import           Data.ByteString.Lazy                            (ByteString)
import qualified Data.ByteString.Lazy.Char8                      as BSL
import           Data.Foldable                                   (traverse_)
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Text                                       (Text)
import           Language.Plutus.Contract                        (Contract,
                                                                  HasBlockchainActions)
import           Language.Plutus.Contract.Effects.ExposeEndpoint (EndpointDescription(getEndpointDescription))
import Ledger.Crypto (pubKeyHash)
import           Ledger.Value                                    (Value)
import           Playground.Types                                (ContractCall (AddBlocks, AddBlocksUntil, CallEndpoint, PayToWallet),
                                                                  EvaluationResult(..), Expression,
                                                                  FunctionSchema (FunctionSchema),
                                                                  PlaygroundError (JsonDecodingError, OtherError),
                                                                  SimulatorWallet (SimulatorWallet), amount, argument,
                                                                  argumentValues, caller, decodingError,
                                                                  endpointDescription,
                                                                  expected, input, recipient,
                                                                  sender,
                                                                  simulatorWalletWallet)
import Plutus.Trace (Trace, ContractConstraints, ContractInstanceTag)
import Plutus.Trace.Playground (Playground, runPlaygroundStream, EmulatorConfig(..), walletInstanceTag)
import qualified Plutus.Trace.Playground as Trace
import Wallet.Emulator.Folds (EmulatorEventFoldM)
import qualified Wallet.Emulator.Folds as Folds
import Wallet.Emulator.Stream (foldEmulatorStreamM, takeUntilSlot)
import           Wallet.Emulator.Types                           (Wallet, walletPubKey)

-- | Unfortunately any uncaught errors in the interpreter kill the
-- thread that is running it rather than returning the error. This
-- means we need to handle all expected errors in the expression we
-- are interpreting. This gets a little tricky because we have to
-- decode JSON inside the interpreter (since we don't have access to
-- it's type outside) so we need to wrap the @apply functions up in
-- something that can throw errors.
playgroundDecode ::
       FromJSON a => String -> ByteString -> Either PlaygroundError a
playgroundDecode expected input =
    first
        (\err ->
             JsonDecodingError
                 {expected, input = BSL.unpack input, decodingError = err}) $
    eitherDecode input

funds :: [Wallet] -> EmulatorEventFoldM effs (Map Wallet Value)
funds = L.generalize . sequenceA . Map.fromList . fmap (\w -> (w, Folds.walletFunds w))

renderInstanceTrace :: [ContractInstanceTag] -> EmulatorEventFoldM effs Text
renderInstanceTrace = 
    L.generalize
    . fmap (renderStrict . layoutPretty defaultLayoutOptions . vsep . fmap pretty)
    . sequenceA
    . fmap Folds.instanceLog

evaluationResultFold :: [Wallet] -> EmulatorEventFoldM effs EvaluationResult
evaluationResultFold wallets =
    let pkh wallet = (pubKeyHash (walletPubKey wallet), wallet)
    in EvaluationResult
            <$> L.generalize Folds.blockchain
            <*> L.generalize Folds.annotatedBlockchain
            <*> L.generalize Folds.emulatorLog
            <*> renderInstanceTrace (walletInstanceTag <$> wallets)
            <*> fmap (fmap (uncurry SimulatorWallet) . Map.toList) (funds wallets)
            <*> pure (fmap pkh wallets)

-- | Evaluate a JSON payload from the Playground frontend against a given contract schema.
stage ::
       forall s a.
       ( HasBlockchainActions s
       , ContractConstraints s
       )
    => Contract s Text a
    -> BSL.ByteString
    -> BSL.ByteString
    -> Either PlaygroundError EvaluationResult
stage contract programJson simulatorWalletsJson = do
    simulationJson :: String <- playgroundDecode "String" programJson
    simulation :: [Expression] <-
        playgroundDecode "[Expression schema]" . BSL.pack $ simulationJson
    simulatorWallets :: [SimulatorWallet] <-
        playgroundDecode "[SimulatorWallet]" simulatorWalletsJson
    let config = EmulatorConfig (toInitialDistribution simulatorWallets)
        allWallets = simulatorWalletWallet <$> simulatorWallets
        final = run
            $ runError
            $ foldEmulatorStreamM @'[Error PlaygroundError] (evaluationResultFold allWallets)
            $ takeUntilSlot 20 -- FIXME: Max slot
            $ runPlaygroundStream config (void contract) (traverse_ expressionToTrace simulation)
    
    case final of
        Left err     -> Left . OtherError . show $ err
        Right result -> Right (fst' result)

toInitialDistribution :: [SimulatorWallet] -> Map Wallet Value
toInitialDistribution = Map.fromList . fmap (\(SimulatorWallet w v) -> (w, v))

expressionToTrace :: Expression -> Eff '[Trace Playground] ()
expressionToTrace = \case
    AddBlocks blcks -> void $ Trace.waitNSlots $ fromIntegral blcks
    AddBlocksUntil slot -> void $ Trace.waitUntilSlot slot
    PayToWallet {sender, recipient, amount} -> void $ Trace.payToWallet sender recipient amount
    CallEndpoint {caller, argumentValues=FunctionSchema { endpointDescription, argument = rawArgument}} ->
        Trace.callEndpoint caller (getEndpointDescription endpointDescription) rawArgument
