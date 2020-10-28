{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module Language.PlutusTx.Plugin (plugin, plc) where

import           Data.Bifunctor
import           Language.PlutusTx.Code
import           Language.PlutusTx.Compiler.Builtins
import           Language.PlutusTx.Compiler.Error
import           Language.PlutusTx.Compiler.Expr
import           Language.PlutusTx.Compiler.Types
import           Language.PlutusTx.Compiler.Utils
import           Language.PlutusTx.PIRTypes
import           Language.PlutusTx.PLCTypes
import           Language.PlutusTx.Plugin.Utils

import qualified GhcPlugins                             as GHC
import qualified Panic                                  as GHC

import qualified Language.PlutusCore                    as PLC
import qualified Language.PlutusCore.Constant.Dynamic   as PLC
import           Language.PlutusCore.Quote

import qualified Language.UntypedPlutusCore             as UPLC

import qualified Language.PlutusIR                      as PIR
import qualified Language.PlutusIR.Compiler             as PIR
import qualified Language.PlutusIR.Compiler.Definitions as PIR

import           Language.Haskell.TH.Syntax             as TH hiding (lift)

import           Codec.Serialise                        (serialise)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.ByteString.Unsafe                 as BSUnsafe
import qualified Data.Map                               as Map
import qualified Data.Text.Prettyprint.Doc              as PP
import           Data.Traversable
import qualified FamInstEnv                             as GHC


import           System.IO.Unsafe                       (unsafePerformIO)

data PluginOptions = PluginOptions {
    poDoTypecheck    :: Bool
    , poDeferErrors  :: Bool
    , poContextLevel :: Int
    , poDumpPir      :: Bool
    , poDumpPlc      :: Bool
    , poOptimize     :: Bool
    }

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin { GHC.pluginRecompile = GHC.flagRecompile
                           , GHC.installCoreToDos = install
                           }
    where
      install :: [GHC.CommandLineOption] -> [GHC.CoreToDo] -> GHC.CoreM [GHC.CoreToDo]
      install args rest = do
          -- create simplifier pass to be placed at the front
          simplPass <- mkSimplPass <$> GHC.getDynFlags
          -- instantiate our plugin pass
          pluginPass <- mkPluginPass
                       <$> parsePluginArgs args
                       <*> getMarkerName
          -- return the pipeline
          pure $
             simplPass
             : pluginPass
             : rest

      -- <https://hackage.haskell.org/package/ghc-8.10.1/docs/GhcPlugins.html#v:thNameToGhcName> says
      -- that we only have to call this once, since it is bound to an exact th symbol (using the 'quote syntax)
      getMarkerName :: GHC.CoreM (Maybe GHC.Name)
      getMarkerName = GHC.thNameToGhcName 'plc


-- | Parses the arguments that were given to ghc at commandline as "-fplugin-optLanguage.PlutusTx.Plugin:arg1"
parsePluginArgs :: [GHC.CommandLineOption] -> GHC.CoreM PluginOptions
parsePluginArgs args = do
    let opts = PluginOptions {
            poDoTypecheck = notElem "dont-typecheck" args
            , poDeferErrors = elem "defer-errors" args
            , poContextLevel = if elem "no-context" args then 0 else if elem "debug-context" args then 3 else 1
            , poDumpPir = elem "dump-pir" args
            , poDumpPlc = elem "dump-plc" args
            , poOptimize = notElem "dont-optimize" args
            }
    -- TODO: better parsing with failures
    pure opts



{- Note [Making sure unfoldings are present]
Our plugin runs at the start of the Core pipeline. If we look around us, we will find
that as expected, we have unfoldings for some bindings from other modules or packages
depending on whether GHC thinks they're good to inline/are marked INLINEABLE.

But there will be no unfoldings for local bindings!

It turns out that these are added by the simplifier, of all things. To avoid relying too
much on the shape of the subsequent passes, we add a single, very gentle, simplifier
pass before we run, turning off everything that we can and running only once.

This means that we need to be robust to the transformations that the simplifier performs
unconditionally which we pretty much are.

See https://gitlab.haskell.org/ghc/ghc/issues/16615 for upstream discussion.
-}

-- Instantiates our plugin given the plugin options and the found marker symbol
mkPluginPass :: PluginOptions -> Maybe GHC.Name -> GHC.CoreToDo
mkPluginPass opts = \case
    Just markerName -> GHC.CoreDoPluginPass "Core to PLC" $
                  let ?markerName = markerName
                      ?opts = opts
                  in compileMod
    Nothing -> GHC.CoreDoNothing -- nothing to do

-- | The plugin works at haskell-module level granularity; the plugin
-- looks at the module's the top-level bindings for plc markers and compile their inner core-expressions.
compileMod :: (?markerName :: GHC.Name, ?opts :: PluginOptions)
           => GHC.ModGuts -> GHC.CoreM GHC.ModGuts
compileMod guts = do
    -- Family env code borrowed from SimplCore
    p_fam_env <- GHC.getPackageFamInstEnv
    let ?famEnvs = (p_fam_env, GHC.mg_fam_inst_env guts)
      -- start looking for plc calls from the top-level binds
      in GHC.bindsOnlyPass (runECore . mapM compileBind) guts

-- | The monad where the plugin runs in for each module.
-- It is a core->core compiler monad, called CoreM, augmented with pure errors.
type ECore uni = ExceptT (CompileError uni) GHC.CoreM
runECore :: ECore PLC.DefaultUni a -> GHC.CoreM a
runECore = runExceptT
          >=> either (failCompilation . show . PP.pretty) pure

{- Note [Hooking in the plugin]
Working out what to process and where to put it is tricky. We are going to turn the result in
to a 'CompiledCode', not the Haskell expression we started with!

Currently we look for calls to the 'plc :: a -> CompiledCode' function, and we replace the whole application with the
generated code object, which will still be well-typed.
-}

{- Note [Polymorphic values and Any]
If you try and use the plugin on a polymorphic expression, then GHC will replace the quantified types
with 'Any' and remove the type lambdas. This is pretty annoying, and I don't entirely understand
why it happens, despite poking around in GHC a fair bit.

Possibly it has to do with the type that is given to 'plc' being unconstrained, resulting in GHC
putting 'Any' there, and that then propagating into the type of the quote. It's tricky to experiment
with this, since you can't really specify a polymorphic type in a type application or in the resulting
'CompiledCode' because that's impredicative polymorphism.
-}



-- | Compiles all the marked expressions in the given binder into PLC literals.
compileBind :: (?markerName :: GHC.Name, ?opts :: PluginOptions, ?famEnvs :: GHC.FamInstEnvs)
            => GHC.CoreBind -> ECore PLC.DefaultUni GHC.CoreBind
compileBind = \case
    GHC.NonRec b rhs -> GHC.NonRec b <$> compileMarkers rhs
    GHC.Rec bindsRhses -> GHC.Rec <$> for bindsRhses
                         (\(b,rhs) -> (b,) <$> compileMarkers rhs)

-- | Compiles all the core-expressions surrounded by plc in the given expression into PLC literals.
compileMarkers :: (?markerName :: GHC.Name, ?opts :: PluginOptions, ?famEnvs :: GHC.FamInstEnvs)
             => GHC.CoreExpr -> ECore PLC.DefaultUni GHC.CoreExpr
compileMarkers = \case
      GHC.App (GHC.App (GHC.App (GHC.App
                          -- function id
                          -- sometimes GHCi sticks ticks around this for some reason
                          (stripTicks -> (GHC.Var fid))
                          -- first type argument, must be a string literal type
                          (GHC.Type (GHC.isStrLitTy -> Just fs_locStr)))
                     -- second type argument
                     (GHC.Type codeTy))
            _)
            -- value argument
            inner
          | ?markerName == GHC.idName fid -> compileMarkedExpOrDefer (show fs_locStr) codeTy inner
      e@(GHC.Var fid) | ?markerName == GHC.idName fid -> throwError . NoContext . InvalidMarkerError $ GHC.ppr e
      GHC.App e a -> GHC.App <$> compileMarkers e <*> compileMarkers a
      GHC.Lam b e -> GHC.Lam b <$> compileMarkers e
      GHC.Let bnd e -> GHC.Let <$> compileBind bnd <*> compileMarkers e
      GHC.Case e b t alts -> do
            e' <- compileMarkers e
            let expAlt (a, bs, rhs) = (,,) a bs <$> compileMarkers rhs
            alts' <- mapM expAlt alts
            pure $ GHC.Case e' b t alts'
      GHC.Cast e c -> flip GHC.Cast c <$> compileMarkers e
      GHC.Tick t e -> GHC.Tick t <$> compileMarkers e
      e@(GHC.Coercion _) -> pure e
      e@(GHC.Lit _) -> pure e
      e@(GHC.Var _) -> pure e
      e@(GHC.Type _) -> pure e

-- | Compile the core expression that is surrounded by a 'plc' marker,
-- and return the plc compiled "bytecode" as a bytestring, to be injected back to the Haskell program.
--
-- If plugin-user has enabled 'defer-errors' and the PIR/PLC compilers have errored during typechecking/compilation,
-- then the function returns a core expression that corresponds to a runtime error, to be injected back to the Haskell program.
compileMarkedExpOrDefer :: (?opts :: PluginOptions, ?famEnvs :: GHC.FamInstEnvs)
               => String -> GHC.Type -> GHC.CoreExpr -> ECore PLC.DefaultUni GHC.CoreExpr
compileMarkedExpOrDefer locStr codeTy origE =
    compileMarkedExpr locStr codeTy origE
    & if poDeferErrors ?opts
      then (`catchError` emitRuntimeError)
      else id
  where
    emitRuntimeError :: CompileError PLC.DefaultUni -> ECore uni GHC.CoreExpr
    emitRuntimeError e = do
          let shown = show $ PP.pretty (pruneContext (poContextLevel ?opts) e)
          defUni <- lift . GHC.lookupTyCon =<< thNameToGhcNameOrFail ''PLC.DefaultUni
          tcName <- thNameToGhcNameOrFail ''CompiledCode
          tc <- lift $ GHC.lookupTyCon tcName
          let args = [GHC.mkTyConTy defUni, codeTy]
          pure $ GHC.mkRuntimeErrorApp GHC.rUNTIME_ERROR_ID (GHC.mkTyConApp tc args) shown

-- | Actually invokes the Core to PIR to PLC compiler to compile an expression into a PLC literal.
-- Compile the core expression that is surrounded by a 'plc' marker,
-- and return the plc compiled ""bytecode" as a bytestring, to be injected back to the Haskell program
compileMarkedExpr :: (?opts :: PluginOptions, ?famEnvs :: GHC.FamInstEnvs)
                => String -> GHC.Type -> GHC.CoreExpr -> ECore PLC.DefaultUni GHC.CoreExpr
compileMarkedExpr locStr codeTy origE = do
    -- module initialization
    flags <- GHC.getDynFlags
    nameInfo <- makePrimitiveNameInfo builtinNames
    let ctx = CompileContext {
            ccOpts = CompileOptions {},
            ccFlags = flags,
            ccFamInstEnvs = ?famEnvs,
            ccBuiltinNameInfo = nameInfo,
            ccBuiltinMeanings = PLC.getStringBuiltinMeanings,
            ccScopes = initialScopeStack,
            ccBlackholed = mempty
            }

    (pirP,uplcP) <- runQuoteT . flip runReaderT ctx $ withContextM 1 (sdToTxt $ "Compiling expr at" GHC.<+> GHC.text locStr) $ runCompiler ?opts origE

    -- serialize the PIR and PLC outputs into a bytestring.
    bsPir <- makeByteStringLiteral . BSL.toStrict $ serialise pirP
    bsPlc <- makeByteStringLiteral . BSL.toStrict $ serialise uplcP

    builder <- lift . GHC.lookupId =<< thNameToGhcNameOrFail 'mkCompiledCode

    -- inject the two bytestrings back as Haskell code.
    pure $
        GHC.Var builder
        `GHC.App` GHC.Type codeTy
        `GHC.App` bsPlc
        `GHC.App` bsPir

-- The GHC.Core to PIR to PLC compiler pipeline. Returns both the PIR and PLC output.
runCompiler
    :: forall uni m . (uni ~ PLC.DefaultUni, MonadReader (CompileContext uni) m, MonadQuote m, MonadError (CompileError uni) m, MonadIO m)
    => PluginOptions
    -> GHC.CoreExpr
    -> m (PIRProgram uni, UPLCProgram uni)
runCompiler opts expr = do
    -- trick here to take out the concrete plc.error
    tcConfigConcrete <-
        runExceptT (PLC.TypeCheckConfig <$> PLC.getStringBuiltinTypes PIR.noProvenance)

    -- turn the concrete plc.error into our compileerror monad
    stringBuiltinTCConfig <- liftEither $ first (view (re PIR._PLCError)) tcConfigConcrete

    let ctx = PIR.defaultCompilationCtx
              & set (PIR.ccOpts . PIR.coOptimize) (poOptimize opts)
              & set PIR.ccBuiltinMeanings PLC.getStringBuiltinMeanings
              & set PIR.ccTypeCheckConfig (
                  if poDoTypecheck opts
                  then Just $ PIR.PirTCConfig stringBuiltinTCConfig PIR.YesEscape
                  else Nothing)

    pirT <- PIR.runDefT () $ compileExprWithDefs expr

    -- We manually run a simplifier+floating pass here before dumping/storing the PIR
    pirT' <- flip runReaderT ctx $ PIR.compileToReadable pirT
    let pirP = PIR.Program () . void $ pirT'

    when (poDumpPir opts) . liftIO . print . PP.pretty $ pirP

    (plcP::PLCProgram PLC.DefaultUni) <- PLC.Program () (PLC.defaultVersion ()) . void <$> (flip runReaderT ctx $ PIR.compileReadableToPlc pirT')
    when (poDumpPlc opts) . liftIO . print $ PP.pretty plcP

    -- We do this after dumping the programs so that if we fail typechecking we still get the dump
    -- again trick to take out the concrete plc.error and lift it into our compileeerror
    when (poDoTypecheck opts) . void $ do
        tcConcrete <- runExceptT $ PLC.typecheckPipeline stringBuiltinTCConfig plcP
        -- also wrap the PLC Error annotations into Original provenances, to match our expected compileerror
        liftEither $ first (view (re PIR._PLCError) . fmap PIR.Original) tcConcrete

    let uplcP = UPLC.eraseProgram plcP
    pure (pirP, uplcP)


-- Helpers
---------

failCompilation :: String -> GHC.CoreM a
failCompilation message = liftIO $ GHC.throwGhcExceptionIO $ GHC.ProgramError $ messagePrefix ++ ": " ++ message
    where messagePrefix = "GHC Core to PLC plugin"

-- | Get the 'GHC.Name' corresponding to the given 'TH.Name', or throw an exception if we can't get it.
thNameToGhcNameOrFail :: TH.Name -> ECore uni GHC.Name
thNameToGhcNameOrFail name = do
    maybeName <- lift $ GHC.thNameToGhcName name
    case maybeName of
        Just n  -> pure n
        Nothing -> throwError . NoContext $ CoreNameLookupError name -- failCompilation $ "Unable to get Core name needed for the plugin to function: " ++ show name

-- | Create a GHC Core expression that will evaluate to the given ByteString at runtime.
makeByteStringLiteral :: BS.ByteString -> ECore uni GHC.CoreExpr
makeByteStringLiteral bs = do
    flags <- GHC.getDynFlags

    {-
    This entire section will crash horribly in a number of circumstances. Such is life.
    - If any of the names we need can't be found as GHC Names
    - If we then can't look up those GHC Names to get their IDs/types
    - If we make any mistakes creating the Core expression
    -}

    -- Get the names of functions/types that we need for our expression
    upio <- lift . GHC.lookupId =<< thNameToGhcNameOrFail 'unsafePerformIO
    bsTc <- lift . GHC.lookupTyCon =<< thNameToGhcNameOrFail ''BS.ByteString
    upal <- lift . GHC.lookupId =<< thNameToGhcNameOrFail 'BSUnsafe.unsafePackAddressLen

    -- We construct the following expression:
    -- unsafePerformIO $ unsafePackAddressLen <length as int literal> <data as string literal address>
    -- This technique gratefully borrowed from the file-embed package

    -- The flags here are so GHC can check whether the int is in range for the current platform.
    let lenLit = GHC.mkIntExpr flags $ fromIntegral $ BS.length bs
    -- This will have type Addr#, which is right for unsafePackAddressLen
    let bsLit = GHC.Lit (GHC.LitString bs)
    let upaled = GHC.mkCoreApps (GHC.Var upal) [lenLit, bsLit]
    let upioed = GHC.mkCoreApps (GHC.Var upio) [GHC.Type (GHC.mkTyConTy bsTc), upaled]

    pure upioed

-- | Strips all enclosing 'GHC.Tick's off an expression.
stripTicks :: GHC.CoreExpr -> GHC.CoreExpr
stripTicks = \case
    GHC.Tick _ e -> stripTicks e
    e            -> e


-- Helper to avoid doing too much construction of Core ourselves
mkCompiledCode :: forall a . BS.ByteString -> BS.ByteString -> CompiledCode PLC.DefaultUni a
mkCompiledCode plcBS pirBS = SerializedCode plcBS (Just pirBS)


mkSimplPass :: GHC.DynFlags -> GHC.CoreToDo
mkSimplPass flags =
  -- See Note [Making sure unfoldings are present]
  GHC.CoreDoSimplify 1 $ GHC.SimplMode {
              GHC.sm_names = ["Ensure unfoldings are present"]
            , GHC.sm_phase = GHC.InitialPhase
            , GHC.sm_dflags = flags
            , GHC.sm_rules = False
            -- You might think you would need this, but apparently not
            , GHC.sm_inline = False
            , GHC.sm_case_case = False
            , GHC.sm_eta_expand = False
            }

-- | Make a 'BuiltinNameInfo' mapping the given set of TH names to their
-- 'GHC.TyThing's for later reference.
makePrimitiveNameInfo :: [TH.Name] -> ECore uni BuiltinNameInfo
makePrimitiveNameInfo names = do
    infos <- for names $ \name -> do
        ghcName <- thNameToGhcNameOrFail name
        thing <- lift $ GHC.lookupThing ghcName
        pure (name, thing)
    pure $ Map.fromList infos
