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
    ) where

import Control.Monad.Freer (Eff, run)
import           Control.Monad.Freer.Error                       (Error, runError)
import Control.Monad (void)
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
import           Ledger.Value                                    (Value)
import           Playground.Types                                (ContractCall (AddBlocks, AddBlocksUntil, CallEndpoint, PayToWallet),
                                                                  EvaluationResult, Expression,
                                                                  FunctionSchema (FunctionSchema),
                                                                  PlaygroundError (JsonDecodingError, OtherError),
                                                                  SimulatorWallet (SimulatorWallet), amount, argument,
                                                                  argumentValues, caller, decodingError,
                                                                  endpointDescription,
                                                                  expected, input, recipient,
                                                                  sender,
                                                                  simulatorWalletWallet)
import Plutus.Trace (Trace, ContractConstraints)
import Plutus.Trace.Playground (Playground, runPlayroundStream)
import qualified Plutus.Trace.Playground as Trace
import Wallet.Emulator.Folds (EmulatorEventFoldM)
import Wallet.Emulator.Stream (foldEmulatorStreamM, takeUntilSlot)
import           Wallet.Emulator.Types                           (Wallet)

-- | Unfortunately any uncaught errors in the interpreter kill the
-- thread that is running it rather than returning the error. This
-- means we need to handle all expected errors in the expression we
-- are interpreting. This gets a little tricky because we have to
-- decode JSON inside the interpreter (since we don't have access to
-- it's type outside) so we need to wrap the @apply functions up in
-- something that can throw errors.
-- type TraceResult
--      = ( Blockchain
--        , [EmulatorEvent]
--        , Text
--        , [SimulatorWallet]
--        , [(PubKey, Wallet)])

-- postProcessEvaluation :: TraceResult -> Either PlaygroundError EvaluationResult
-- postProcessEvaluation (resultBlockchain, emulatorLog, emulatorTrace, fundsDistribution, walletKeys) = do
--     rollup <- first RollupError $ doAnnotateBlockchain resultBlockchain
--     pure $
--         EvaluationResult
--             { resultBlockchain
--             , resultRollup = rollup
--             , emulatorLog
--             , emulatorTrace
--             , fundsDistribution
--             , walletKeys = first pubKeyHash <$> walletKeys
--             }

playgroundDecode ::
       FromJSON a => String -> ByteString -> Either PlaygroundError a
playgroundDecode expected input =
    first
        (\err ->
             JsonDecodingError
                 {expected, input = BSL.unpack input, decodingError = err}) $
    eitherDecode input

evaluationResultFold :: EmulatorEventFoldM effs EvaluationResult
evaluationResultFold = undefined

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
    let allWallets = simulatorWalletWallet <$> simulatorWallets
        config = undefined allWallets -- fixme intiial distribution
        final = run
            $ runError
            $ foldEmulatorStreamM @'[Error PlaygroundError] evaluationResultFold
            $ takeUntilSlot 20 -- FIXME: Max slot
            $ runPlayroundStream config (void contract) (traverse_ expressionToTrace simulation)
    
    case final of
        Left err     -> Left . OtherError . show $ err
        Right result -> Right (fst' result)

-- TODO:
-- buildSimulation :: [Wallet] -> [Expression] -> Trace Emulator
-- where we
-- 1. Start the contract for all simulated wallets
-- 2. Store the handles in a map
-- 3. turn analyzeEmulatorState into a fold of emulator events
-- 4. Run the trace and fold the result

toInitialDistribution :: [SimulatorWallet] -> Map Wallet Value
toInitialDistribution = Map.fromList . fmap (\(SimulatorWallet w v) -> (w, v))

expressionToTrace :: Expression -> Eff '[Trace Playground] ()
expressionToTrace = \case
    AddBlocks blcks -> void $ Trace.waitNSlots $ fromIntegral blcks
    AddBlocksUntil slot -> void $ Trace.waitUntilSlot slot
    PayToWallet {sender, recipient, amount} -> void $ Trace.payToWallet sender recipient amount
    CallEndpoint {caller, argumentValues=FunctionSchema { endpointDescription, argument = rawArgument}} ->
        Trace.callEndpoint caller (getEndpointDescription endpointDescription) rawArgument
