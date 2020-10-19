module TH.Docs (genDocs) where

import Language.Haskell.TH

mkTySyn :: (Word,Name) -> Dec
mkTySyn (code,e) = TySynD (mkName $ "E" ++ show code) [] $ ConT e

genDocs :: [(Word,Name)] -> Q [Dec]
genDocs = pure . fmap mkTySyn
