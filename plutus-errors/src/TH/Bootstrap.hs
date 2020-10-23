module TH.Bootstrap (
    bootstrap
    ) where

import Language.Haskell.TH
import Language.Plutus.Common
import Data.Traversable
import Data.List
import Data.Function
import Data.Foldable
import qualified Data.Map as M

-- | Given a list of errors (data constructors), it returns
-- ErrorCode instances to bootstrap and paste the error codes
bootstrap :: [Name] -> Q [Dec]
bootstrap es = do
    -- give them a unique number
    let indexedEs = zip es [1..]
    -- group them by their parent type
    grouppedByParentType <- groupDataConstrs indexedEs
    M.elems <$> M.traverseWithKey makeInstance grouppedByParentType

    where
      groupDataConstrs :: [IxError] -> Q (M.Map ParentName [IxError])
      groupDataConstrs ns = foldlM (\ acc e -> do
        DataConI _ _ parentName <- reify $ fst e
        pure $ M.insertWith (++) parentName [e] acc
        ) M.empty ns

      makeInstance :: ParentName -> [IxError] -> Q Dec
      makeInstance parentName ies = do
          k <- reifyType parentName
          appliedTy <- genSaturatedTypeCon parentName k
          pure $ InstanceD Nothing [] (AppT (ConT (mkName "ErrorCode")) appliedTy)
            [FunD (mkName "errorCode") $
               fmap (\ (d,i) -> Clause [RecP d []] (NormalB $ LitE $ IntegerL i) []) ies
               ++ [errorCodeCatchAllFun]
            ]

      errorCodeCatchAllFun :: Clause
      errorCodeCatchAllFun = Clause [WildP] (NormalB $ LitE $ IntegerL 0) []


-- | generate saturatedtypecon given its name and its kind
genSaturatedTypeCon :: Name -> Type -> Q Type
genSaturatedTypeCon = genSaturatedTypeCon' 1
 where
   genSaturatedTypeCon' :: Int -> Name -> Type -> Q Type
   genSaturatedTypeCon' ix tn (AppT _ k) = do
    freshTyVar <- newName $ "_a" ++ show ix
    rest <- genSaturatedTypeCon' (ix+1) tn k
    pure $ AppT rest (VarT freshTyVar)
   genSaturatedTypeCon' _ tn _ = pure $ ConT tn

type IxError = (Name,Integer)
