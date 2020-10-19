{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_HADDOCK ignore_exports #-}
module Docs where

import TH.Docs
import Errors
import Codes

$(genDocs (zip codes errors))

