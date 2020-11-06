{-# LANGUAGE TemplateHaskell #-}
module Codes (codes) where

import TH.Codes
import Errors
import Numeric.Natural

codes :: [Natural]
codes = $(genCodes errors)
