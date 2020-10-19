{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main where


import AllErrors
import TH.Bootstrap

$(bootstrap allErrors)


{- run as:

> cabal exec -- runghc scripts/Bootstrapper.hs > scripts/Bootstrapper.stdout

and take the output

-}
