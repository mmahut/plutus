{-# LANGUAGE TemplateHaskell #-}
module TH.GenCodes (genCodes) where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Datatype as TH
import Data.Traversable
import ErrorCode

-- | Takes the names of all errors/dataconstructors in a list
-- and constructs a list hs-experession containing
-- allthe error codes :: [Natural]
genCodes :: [TH.Name] -> Q TH.Exp
genCodes cs = do
   method <- [| errorCode |]    -- the errorCode method
   undef <- [| undefined |]   -- the 'undefined' exp
   fmap ListE . for cs $ \ c -> do
       cInfo <- TH.reifyConstructor c
       pure $ method `AppE` genSaturatedCon cInfo undef

-- | Given some data-constructor info (number of args and name),
-- generate a fully-applied (saturated) value of the constructor by
-- fully applying it to `undefined` values.
-- e.g. `(Nothing), (Just undefined), (undefined,undefined,undefined), etc`
-- Note: breaks on data-constructors that have bangs.
genSaturatedCon :: TH.ConstructorInfo -> TH.Exp -> TH.Exp
genSaturatedCon cInfo undef =
    foldr (const $ \ acc -> acc `AppE` undef)
    (ConE $ constructorName cInfo) $ constructorFields cInfo
