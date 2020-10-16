{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion.Main
import           Criterion.Types                                            (Config (..))

import           Codec.Serialise
import           Control.Monad
import           Control.Monad.Trans.Except                                 (runExceptT)
import qualified Data.ByteString.Lazy                                       as BSL
import           Data.Functor                                               ((<&>))
import qualified Data.Map                                                   as Map
import           System.FilePath
import           Text.Printf                                                (printf)

import qualified Language.PlutusCore                                        as PLC
import           Language.PlutusCore.CBOR
import           Language.PlutusCore.Constant                               (DynamicBuiltinNameMeanings (..))
import           Language.PlutusCore.Constant.Dynamic
import           Language.PlutusCore.Evaluation.Machine.ExBudgetingDefaults
import qualified Language.PlutusCore.Universe                               as PLC
import qualified Language.UntypedPlutusCore                                 as UPLC
import qualified Language.UntypedPlutusCore.DeBruijn                        as UPLC
import qualified Language.UntypedPlutusCore.Evaluation.Machine.Cek          as UPLC

{-- | This set of benchmarks is based on validations occurring in the tests in
  plutus-use-cases.  Those tests are run on the blockchain simulator, and a
  modified version of Ledger.Scripts was used to extract validator scripts and
  their arguments.  These are stored in the `data` directory in CBOR form, along
  with README files explaining which scripts were involved in each validation
  during the tests.  --}

type Term a    = UPLC.Term PLC.Name PLC.DefaultUni a
type Program a = UPLC.Program PLC.Name PLC.DefaultUni a

{- Generate an HTML report.  If run via stack/cabal this will be written to the
   `plutus-benchmark` directory by default.  The -o option can be used to change this,
   but an absolute path will  probably be required (eg, "-o=$PWD/report.html") . -}
config :: Config
config = defaultConfig
  { reportFile = Just "report.html"
  , template = "./default.tpl"  -- Include total number of iterations in HTML report
  }

fromDeBruijn ::  UPLC.Program UPLC.DeBruijn PLC.DefaultUni a ->  IO (Program a)
fromDeBruijn prog = do
    let namedProgram = UPLC.programMapNames (\(UPLC.DeBruijn ix) -> UPLC.NamedDeBruijn "v" ix) prog
    case PLC.runQuote $ runExceptT $ UPLC.unDeBruijnProgram namedProgram of
      Left e  -> error $ show e
      Right p -> return p

loadPlc :: Serialise a => FilePath -> IO (Program a)
loadPlc file = do
  BSL.readFile file <&> deserialiseOrFail >>= mapM fromDeBruijn >>= \case
               Left (DeserialiseFailure offset msg) ->
                   error ("Deserialisation failure at offset " ++ Prelude.show offset ++ ": " ++ msg)
               Right r -> return r

benchCek :: Term () -> Benchmarkable
benchCek program = nf (UPLC.unsafeEvaluateCek getStringBuiltinMeanings defaultCostModel) program


cborSuffix :: String
cborSuffix = "cbor"

-- The directory containing the scripts.  This will be relative to the working
-- directory, which will be `plutus-benchmark` if the benchmarks are being run
-- via cabal or stack.
dataDir :: String
dataDir = "bench-validation" </> "data"

{- Construct an applied validator.  We assume that relevant validators, datum
   scripts, redeemers and contexts are stored in CBOR format under `<progName>`
   in the `data` directory.  These should have names like "Redeemer01.cbor",
   "Context03.cbor", and so on. This function returnes a Criterion environment
   to be fed to the relevant benchmark, to keep the IO overhead out of the
   benchmark itself. -}
getAppliedScript :: String -> Int -> Int -> Int -> Int -> IO (Term ())
getAppliedScript progName validatorNumber datumNumber redeemerNumber contextNumber = do
  let dataPath = dataDir </> progName
      loadScript base scriptNumber = do
          let file = dataPath </> (base ++ printf "%02d" scriptNumber) <.> cborSuffix
          loadPlc file
  validator <- loadScript "Validator" validatorNumber
  datum     <- loadScript "Datum"     datumNumber
  redeemer  <- loadScript "Redeemer"  redeemerNumber
  context   <- loadScript "Context"   contextNumber
  let appliedValidator = validator `UPLC.applyProgram` datum `UPLC.applyProgram` redeemer `UPLC.applyProgram` context
  pure $ void . UPLC.toTerm $ appliedValidator


{- Create a benchmark with a name like "crowdfunding/5" by applying validator
   number v to datum number d, redeemer number r, and context number c in the
   directory data/<dirname>.  The 'id' argument is just to make the names of the
   indvidual benchmarks more readable and more easily typed. -}
mkBM :: String -> (Int, (Int, Int, Int, Int)) -> Benchmark
mkBM dirname (id, (v,d,r,c)) =
    env (getAppliedScript dirname v d r c) $ \ ~ script -> bench (show id) $ benchCek script

-- Make a `bgroup` collecting together a number of benchmarks for the same contract
mkBgroup :: String -> [(Int, (Int, Int, Int, Int))] -> Benchmark
mkBgroup dirname bms = bgroup dirname (map (mkBM dirname) bms)

-- See the README files in the data directories for the combinations of scripts
main :: IO ()
main = defaultMainWith config
       [ mkBgroup
         "crowdfunding"
         [ (1, (1,1,1,1))
         , (2, (1,2,1,2))
         , (3, (1,3,1,3))
         , (4, (1,3,2,4))
         , (5, (1,1,2,5))
         ]
       , mkBgroup
         "future"
         [ (1, (1,1,1,1))
         , (2, (2,2,1,2))
         , (3, (2,3,1,3))
         , (4, (3,4,2,4))
         , (5, (3,5,3,5))
         , (6, (3,4,4,6))
         , (7, (3,4,3,7))
         ]
       , mkBgroup
         "multisigSM"
         [ (1,  (1,1,1,1))
         , (2,  (1,2,2,2))
         , (3,  (1,3,3,3))
         , (4,  (1,4,4,4))
         , (5,  (1,5,5,5))
         , (6,  (1,1,1,6))
         , (7,  (1,2,2,7))
         , (8,  (1,3,3,8))
         , (9,  (1,4,4,9))
         , (10, (1,5,5,10))
         ]
       , mkBgroup
         "vesting"
         [ (1,  (1,1,1,1))
         , (2,  (2,1,1,2))
         , (3,  (3,1,1,1))
         ]
       , mkBgroup
         "marlowe/trustfund"
         [ (1,  (1,1,1,1))
         , (2,  (1,2,2,2))
         ]
       , mkBgroup
         "marlowe/zerocoupon"
         [ (1,  (1,1,1,1))
         , (2,  (1,2,2,2))
         ]
       ]
