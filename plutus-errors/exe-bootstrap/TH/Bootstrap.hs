-- | (Re)generate ErrorCode instances for initial errors
module TH.Bootstrap (
    bootstrap
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.Foldable
import Numeric.Natural
import Data.Map as M

{- |
The purpose of this function is to help in the (re)-generation of 'ErrorCode' instances
for Plutus-errors/data-constructors. The user can call this function as a script to
get the generated instances as Haskell code and paste/modify it accordingly next to the errors (for avoiding orphans).
The function works by assigning a unique number to each dataconstructor, starting counting from 1.
The function groups the data-constructors by their "parent" type-constructor,
so the order that they are given in the input list does not matter.
-}
bootstrap :: [Name] -> Q [Dec]
bootstrap constrs = do
    -- give them a unique numbering
    let indexedCs = zip constrs $ [1::Natural ..]
    -- group them by their parent type
    groups <- groupConstrs indexedCs
    -- generate the instances
    pure . M.elems $ M.mapWithKey makeInstance groups


-- | A dataconstructor representing a plutus error, paired with a generated unique errorcode
type IxCons = (Name, Natural)
-- | An intermediate structure to group data-constructors by their parent type-constructor and number of tyvars
type Group = (ParentName,Int)

-- | Groups a list of (possibly-unrelated) dataconstructors by their parent typeconstructor
groupConstrs :: [IxCons] -> Q (Map Group [IxCons])
groupConstrs ns = foldlM groupByParent mempty ns
  where
    groupByParent :: Map Group [IxCons]
                  -> IxCons -> Q (Map Group [IxCons])
    groupByParent acc indexD = do
        dataInfo <- reifyDatatype $ fst indexD
        pure $ M.insertWith (++) (datatypeName dataInfo, length $ datatypeVars dataInfo) [indexD] acc


-- Create a haskell code declaration corresponding to
-- `instance TyCons _dummyTyVars where errorCode DataCons = ix`
makeInstance :: Group -> [IxCons] -> Dec
makeInstance (parentName, countTyVars) ies =
    let appliedTy = genSaturatedTypeCon parentName countTyVars
    in InstanceD Nothing [] (AppT (ConT (mkName "ErrorCode")) appliedTy)
            [FunD (mkName "errorCode") $
               fmap (\ (d,i) -> Clause [RecP d []] (NormalB $ LitE $ IntegerL $ toInteger i) []) ies
               ++ [errorCodeCatchAllFun]
            ]
    where
      -- Generate a saturated (fully-applied) type constructor
      -- by applying it to somne dummy type variable names.
      genSaturatedTypeCon :: Name -> Int -> Type
      genSaturatedTypeCon tn 0 = ConT tn
      genSaturatedTypeCon tn ix =
          genSaturatedTypeCon tn (ix-1)  `AppT` VarT (mkName $ dummyTyVars !! (ix-1))
        where
          dummyTyVars :: [String]
          dummyTyVars = fmap (\c -> '_':[c]) ['a'..'z'] -- NOTE: breaks for more than 26 tyvars!

      -- a dummy catch-all generated code for convenience
      -- (but leads to unsafety because it makes the method total function)
      errorCodeCatchAllFun :: Clause
      errorCodeCatchAllFun = Clause [WildP] (NormalB $ LitE $ IntegerL 0) []
