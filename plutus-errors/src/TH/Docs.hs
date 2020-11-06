module TH.Docs (genDocs) where

import Language.Haskell.TH
import Numeric.Natural

mkTySyn :: (Natural,Name) -> Dec
mkTySyn (code,e) = TySynD (mkName $ "E" ++ show code) [] $ ConT e

genDocs :: [(Natural,Name)] -> Q [Dec]
genDocs = pure . fmap mkTySyn
