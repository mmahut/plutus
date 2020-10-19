{-# LANGUAGE TemplateHaskell #-}
module Codes (codes) where

import TH.Codes
import Errors

codes :: [Word]
codes = $(genCodes errors)
