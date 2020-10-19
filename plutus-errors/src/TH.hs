module TH where

import Language.Haskell.TH
import PlutusError

createDoc :: [Name] -> Q [Dec]
createDoc ns = do
    pure []

createDoc' :: Name -> Q [Dec]
createDoc' n = do
    let x = errorCode 
    pure []
