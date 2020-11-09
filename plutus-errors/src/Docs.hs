{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
-- | All the Plutus errors project-wise, indexed by their error code.
module Docs () where

import TH.GenDocs
import Errors
import Codes

$(genDocs (zip allErrors allCodes))

