{-# LANGUAGE TemplateHaskell #-}
module Errors (errors) where

import Language.Haskell.TH
import ErrorCode

import qualified Language.PlutusIR.Error as PIR
import qualified Language.PlutusIR.Parser as PIR
import qualified Language.PlutusCore.Error as PLC
import qualified Language.PlutusCore.DeBruijn as PLC
import qualified Language.PlutusCore.Evaluation.Machine.Exception as PLC
import qualified Language.PlutusCore.Evaluation.Machine.Cek as PLC
import qualified Language.UntypedPlutusCore.Evaluation.Machine.Cek as PLCU
import qualified  Language.PlutusTx.Code as PTX
import qualified  Language.PlutusTx.Lift.Class as PTX
import qualified  Language.PlutusTx.Compiler.Error as PTX

{- | A collection of error instances and their codes that are deprecated.

When an error is deprecated (does not trigger anymore) and (some of) its dataconstructors has been removed,
and in case the error is "exposed" to the public, then it is required that its "deprecated" constructors
be "moved" and listed/errorCoded under the umbrella datatype `plutus-errors:Errors.DeprecatedErrors`.

See NOTE [Error Codes of plutus errors]
-}
{-# DEPRECATED DeprecatedErrors "These errors and their error codes *should* not be thrown by any plutus code anymore" #-}
data DeprecatedErrors =
    ReservedErrorCode
    -- append here your deprecated errors

instance ErrorCode DeprecatedErrors where
    errorCode ReservedErrorCode {} = 0

-- | All errors among the whole project categorized. This includes both existing and deprecated errors.
--
-- Note: order of adding to this list does not matter at the moment.
errors :: [Name]
errors =
   [ 'ReservedErrorCode
   , 'PIR.MalformedDataConstrResType
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
   , 'PTX.CompilationError
   , 'PTX.UnsupportedError
   , 'PTX.FreeVariableError
   , 'PTX.UnsupportedLiftType
   , 'PTX.UnsupportedLiftKind
   , 'PTX.UserLiftError
   , 'PTX.LiftMissingDataCons
   , 'PTX.LiftMissingVar
   ]
