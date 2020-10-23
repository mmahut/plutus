module Language.Plutus.Common
    ( ErrorCode(..)
    ) where

{- NOTE [Error Codes of plutus errors]

Our goal is to assign a unique-among-the-project error number (errorCode) to all errors
that might occur during any phase of plutus code --- plutustx th deriving, plugin tx compiling,
pir compiling, plc executing, "offline" runtime plutus code ---, so
as to document and easily identify these plutus errors.

We drew inspiration from `rustc` compiler error-codes:
<https://doc.rust-lang.org/nightly/nightly-rustc/rustc_errors/index.html>

An errorcode is a positive number (`Word`) assigned to every possible data-constructor
that represents an exceptional case. This includes both pure error-values raised
by e.g. `ExceptT` but also "impure" ghc-builtin Control.Exception instances.

For that we created a class `ErrorCode` with one method `errorCode`,
left to be implemented for each error by the Plutus developers.
It is the responsibility of the  Plutus developer to make sure that

1) the assigned errorcode (Word) is unique among the whole Plutus-project codebase,
2) the `errorCode` method is total
3) no "wrapper-constructors" are tagged. e.g.in:

```data PirError =
    WrapperTC PIR.TCError
    | WrapperParse  PIR.ParseError
    | PirCompile String
```

we do not uniquely tag the wrapper-constructors WrapperTC,WrapperParse,WrapperCompile,
we only tag the "base error constructor" PirCompile:

```
instance ErrorCode PirError where
   errorCode PirCompile {} = 9997
   errorCode (WrapperTC e) = errorCode e
   errorCode (WrapperParse e) = errorCode e
```

A Plutus sub-project that wants to throw an error, must depend on this package `plutus-common`.
To aid in defining an instance for a brand-new (uncategorized) error or added error-dataconstructors,
the Plutus developer can make use (but not cabal-depend upon) of the
mega-package plutus-errors to "guess-pick" an error-code that is not currently in use
by the whole codebase, by running

```
> cabal run plutus-errors:next-error.
An error code that is not currently used is: 9998
```

After defining/extending this errorcode instance, the Plutus developer must navigate to the megapackage and
confirm these new errorcodes by adding all newly-introduced base-error constructors
to the list of all-project errors `plutus-errors/src/Errors.hs`. The TH code of `plutus-errors`
will make sure there are not duplicates, and running the `cabal haddock plutus-errors`
will build the documentation of all (categorized) errors.

When an error is deprecated (does not trigger anymore) and (some of) its dataconstructors has been removed,
and in case the error is "exposed" to the public, then it is required that its "deprecated" constructors
be "moved" and listed/errorCoded under the umbrella datatype `plutus-errors:Errors.DeprecatedErrors`.
The reason for this is to document/keep track of deprecated errors and not *re*-pick "old" error-codes.

Currently all errors among the project are placed into one big pile of error-codes. We later
might use sub-groups of error-codes with specific word ranges, e.g. (PIR : 0000 - 0100, PLC: 0100 - 0200, etc), which then would require
to put into use the "wrapper-constructors" of our error-grouppings.
-}

-- | Assigns an error-code (positive number) to data-constructors (values) of error types.
class ErrorCode a where
    errorCode :: a -> Word
    {-# MINIMAL errorCode #-}
