{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Plutus.Trace.Tag(Tag(..)) where

import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text.Prettyprint.Doc (Pretty(..), braces)

-- | A human-readably piece of data, used to identify threads and contract
--   instances
newtype Tag = Tag { unTag :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON, IsString)

instance Pretty Tag where
    pretty = braces . pretty . unTag