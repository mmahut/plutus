module TH.GenDocs (genDocs) where

import Language.Haskell.TH as TH
import Numeric.Natural
import Text.Printf

-- | An alias (type-synonym) for a given error,
-- using its error-code as the alias's name.
mkTySyn :: (TH.Name,Natural) -> TH.Dec
mkTySyn (err, code) =
    let aliasName = mkName $ printf "E%04d" code
    in TySynD aliasName [] $ ConT err

-- | Generate documentation for the given errors and their error-codes.
-- The documentation's generation relies on `DataKinds`, i.e. lifted data-constructors
genDocs :: [(TH.Name,Natural)] -> Q [TH.Dec]
genDocs = pure . fmap mkTySyn
