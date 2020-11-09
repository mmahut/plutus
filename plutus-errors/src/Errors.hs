{-# LANGUAGE TemplateHaskell #-}
-- | The cataloguing of all Plutus errors, obsolete or not.
module Errors (allErrors) where

import Language.Haskell.TH as TH
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

{- | A collection of error instances which are obsolete, together with their error codes bundled to one instance.
See plutus-errors/README.md
-}
{-# WARNING ObsoleteErrors "These errors and their error codes *should* not be thrown by any plutus code anymore" #-}
data ObsoleteErrors =
    ReservedErrorCode
    -- append here the obsolete errors

instance ErrorCode ObsoleteErrors where
    errorCode ReservedErrorCode {} = 0
    -- append here the corresponding obsolete error codes

-- | All errors among the whole Plutus project. This includes both existing and obsolete errors.
-- Note: the order of adding to this list does not matter, except for haddock looks.
allErrors :: [TH.Name]
allErrors =
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
