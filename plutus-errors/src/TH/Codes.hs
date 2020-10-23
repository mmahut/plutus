{-# LANGUAGE TemplateHaskell #-}
module TH.Codes (genCodes) where

import Language.Haskell.TH
import Language.Plutus.Common

genCodes :: [Name] -> Q Exp
genCodes ns = listE $ fmap (\ n -> do
   DataConI n' t _  <- reify n
   fun <- [|errorCode|]
   pure $ AppE fun $ genSaturatedDataCon n' t
   ) ns

genSaturatedDataCon :: Name -> Type -> Exp
genSaturatedDataCon = genSaturatedDataCon' . ConE
    where
      genSaturatedDataCon' acc (ForallT _ _ t) = genSaturatedDataCon' acc t
      genSaturatedDataCon' acc (AppT _ (VarT _)) = acc
      genSaturatedDataCon' acc (AppT _ t') = genSaturatedDataCon' (acc `AppE` VarE (mkName "Prelude.undefined")) t'
      genSaturatedDataCon' acc _ = acc 
