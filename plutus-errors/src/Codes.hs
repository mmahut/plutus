{-# LANGUAGE TemplateHaskell #-}
module Codes (allCodes) where

import TH.GenCodes
import Errors
import Numeric.Natural

-- | All the error-codes in the order of they appear in the 'allErrors' catalogue.
allCodes :: [Natural]
allCodes = $(genCodes allErrors)
