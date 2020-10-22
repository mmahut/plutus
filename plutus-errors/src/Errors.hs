{-# LANGUAGE TemplateHaskell #-}
module Errors (errors) where

import Language.Haskell.TH
import PlutusError

import qualified Language.PlutusIR.Error as PIR
import qualified Language.PlutusIR.Parser as PIR
import qualified Language.PlutusCore.Error as PLC
import qualified Language.PlutusCore.DeBruijn as PLC
import qualified Language.PlutusCore.Evaluation.Machine.Exception as PLC
import qualified Language.PlutusCore.Evaluation.Machine.Cek as PLC
import qualified Language.UntypedPlutusCore.Evaluation.Machine.Cek as PLCU
import qualified  Language.PlutusTx.Code as PTX
import qualified  Language.PlutusTx.Lift.Class as PTX
import qualified Language.PlutusTx.Utils as PTX
import qualified  Language.PlutusTx.Compiler.Error as PTX
import qualified Language.PlutusTx.Compiler.Expr as PTX
import qualified Language.PlutusTx.Compiler.Type as PTX
import qualified Language.PlutusTx.Compiler.Kind as PTX
import qualified Language.PlutusTx.Compiler.Builtins as PTX

-- | A collection of error instances and their codes that are deprecated
{-# DEPRECATED DeprecatedErrors "These errors and their error codes *should* not be thrown by any plutus code anymore" #-}
data DeprecatedErrors =
    ReservedErrorCode
    -- append here your deprecated errors

instance ErrorCode DeprecatedErrors where
    errorCode ReservedErrorCode {} = 0

errors :: [Name]
errors =
   [ 'PIR.MalformedDataConstrResType
   , 'PIR.CompilationError
   , 'PIR.UnsupportedError
   , 'PIR.UnexpectedKeyword
    , 'PIR.InternalError
    , 'PLC.LexErr
    , 'PLC.Unexpected
    , 'PLC.UnknownBuiltinType
    , 'PLC.UnknownBuiltinFunction
    , 'PLC.InvalidBuiltinConstant
    , 'PLC.MultiplyDefined
    , 'PLC.IncoherentUsage
    , 'PLC.BadType
    , 'PLC.BadTerm
    , 'PLC.KindMismatch
    , 'PLC.TypeMismatch
    , 'PLC.UnknownDynamicBuiltinNameErrorE
    , 'PLC.OpenTypeOfBuiltin
    , 'PLC.FreeTypeVariableE
    , 'PLC.FreeVariableE
    , 'PLC.FreeVariable
    , 'PLC.FreeUnique
    , 'PLC.FreeIndex
    , 'PLC.NonPolymorphicInstantiationMachineError
    , 'PLC.NonWrapUnwrappedMachineError
    , 'PLC.NonFunctionalApplicationMachineError
    , 'PLC.OpenTermEvaluatedMachineError
    , 'PLC.TooFewArgumentsConstAppError
    , 'PLC.TooManyArgumentsConstAppError
    , 'PLC.UnliftingErrorE
    , 'PLC.BuiltinTermArgumentExpectedMachineError
    , 'PLC.UnexpectedBuiltinTermArgumentMachineError
    , 'PLC.EmptyBuiltinArityMachineError
    , 'PLC.CekOutOfExError
    , 'PLC.CekEvaluationFailure
    , 'PLCU.CekOutOfExError
    , 'PLCU.CekEvaluationFailure
    , 'PTX.ImpossibleDeserialisationFailure
    -- -- Language.PlutusTx.Lift.Class,Prelude.error $ "Unknown local variable: " ++ show name
    -- -- Language.PlutusTx.Lift.Class,Prelude.error $ "Constructors not created for " ++ show tyName
    -- -- Language.PlutusTx.Lift.Class,dieTH "Newtypes must have a single constructor with a single argument"
    -- -- Language.PlutusTx.Lift.Class,dieTH "Newtypes must have a single constructor with a single argument"
    -- -- Language.PlutusTx.Lift.Class,dieTH $ "Unsupported kind: " ++ show k
    -- -- Language.PlutusTx.Lift.Class,dieTH $ "Unsupported type: " ++ show t
    -- -- Language.PlutusTx.Utils,mustbeReplaced,GHC.Exception.ErrorCall -- for "plutustx" user-error-builtin run by ghc
    , 'PTX.CompilationError
    , 'PTX.UnsupportedError
    , 'PTX.FreeVariableError
    , 'PTX.UnsupportedLiftType
    , 'PTX.UnsupportedLiftKind
    , 'PTX.UserLiftError
    , 'PTX.LiftMissingDataCons
    , 'PTX.LiftMissingVar
    -- -- Language.PlutusTx.Plugin,failCompilation $ "Unable to get Core name needed for the plugin to function: " ++ show name
    , 'ReservedErrorCode
    --, 'PLC.OtherMachineError -- we don't need this one, it is a wrapper
    -- , 'PLC.ConstAppMachineError -- we don't need this one, it is a wrapper
    ]
