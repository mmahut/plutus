module TH.Bootstrap (
    bootstrap
    ) where

import Language.Haskell.TH
import Data.Foldable
import Numeric.Natural
import qualified Data.Map as M

-- | A dataconstructor representing a plutus error, paired with a generated unique errorcode
type IxError = (Name, Natural)

-- | The sole purpose of this function is to help in the (re)-generation of 'ErrorCode' instances
-- for Plutus-errors/data-constructors. The user can call this function as a script to
-- get the generated instances as Haskell code and paste/modify it accordingly next to the errors (for avoiding orphans).
-- The function works by assigning a unique number to each dataconstructor, starting counting from 1.
-- The function groups the data-constructors by their "parent" type-constructor,
-- so the order that they are given in the input list does not matter.
bootstrap :: [Name] -> Q [Dec]
bootstrap dataConstrs = do
    -- give them a unique number
    let indexedDs = zip dataConstrs ([1..] :: [Natural])
    -- group them by their parent type
    groupedByParentType <- groupDataConstrs indexedDs
    M.elems <$> M.traverseWithKey makeInstance groupedByParentType

    where
      groupDataConstrs :: [IxError] -> Q (M.Map ParentName [IxError])
      groupDataConstrs ns = foldlM groupByParent mempty ns

      groupByParent :: M.Map ParentName [IxError]
                    -> IxError -> Q (M.Map ParentName [IxError])
      groupByParent acc indexD = do
        DataConI _ _ parentName <- reify $ fst indexD
        pure $ M.insertWith (++) parentName [indexD] acc

      makeInstance :: ParentName -> [IxError] -> Q Dec
      makeInstance parentName ies = do
          k <- reifyType parentName
          appliedTy <- genSaturatedTypeCon parentName k
          pure $ InstanceD Nothing [] (AppT (ConT (mkName "ErrorCode")) appliedTy)
            [FunD (mkName "errorCode") $
               fmap (\ (d,i) -> Clause [RecP d []] (NormalB $ LitE $ IntegerL $ toInteger i) []) ies
               ++ [errorCodeCatchAllFun]
            ]

      errorCodeCatchAllFun :: Clause
      errorCodeCatchAllFun = Clause [WildP] (NormalB $ LitE $ IntegerL 0) []


-- | Given the name and kind of a typeconstructor, generated a saturated (fully-applied)
-- type constructor by using fresh, unbound type variables.
genSaturatedTypeCon :: Name -> Type -> Q Type
genSaturatedTypeCon = genSaturatedTypeCon' 1
 where
   genSaturatedTypeCon' :: Int -> Name -> Type -> Q Type
   genSaturatedTypeCon' ix tn (AppT _ k) = do
       freshTyVar <- newName $ "_a" ++ show ix
       rest <- genSaturatedTypeCon' (ix+1) tn k
       pure $ AppT rest (VarT freshTyVar)
   genSaturatedTypeCon' _ tn _ = pure $ ConT tn

