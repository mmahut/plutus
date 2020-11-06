{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Docs () where

import TH.Docs
import Errors
import Codes

$(genDocs (zip codes errors))

