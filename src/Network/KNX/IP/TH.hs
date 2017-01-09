module Network.KNX.IP.TH where

import Control.Monad
import Data.Maybe
import Network.KNX.IP.Serialize
import Language.Haskell.TH

mkEnumSerialize :: Name -> [(Name, Integer)] -> Q [Dec]
mkEnumSerialize ty vals = do
  TyConI (DataD _ _ _ _ cons _) <- reify ty
  let ns = map toName cons
  mapM_ (\con -> when (isNothing $ lookup con vals) $ fail $ "Missing constructor " ++ nameBase con) ns
  mapM_ (\(a, _) -> when (a `notElem` ns) $ fail $ "Surplus constructor " ++ nameBase a) vals
  pure <$> instanceD (cxt []) [t|Serialize $(conT ty)|] [mkFrom, mkTo]

  where
    toName (NormalC n _) = n
    toName _ = error "Add support for more Con constructors"
    
    mkFrom = funD (mkName "put") (map mkFromClause vals)
    mkFromClause (con, val) = clause [conP con []]
      (normalB $ appE (varE 'putWord8) (litE $ integerL val)) []

    mkTo = valD (varP $ mkName "get") (normalB [|getWord8 >>= toConstr|])
      [funD (mkName "toConstr") (map mkToClause vals ++ [wildToClause])]
    mkToClause (con, val) = clause [litP $ integerL val]
      (normalB [|pure $(conE con)|]) []
    wildToClause = clause [wildP]
      (normalB [|fail $ "Incorrect tag for " ++ $(stringE (nameBase ty))|]) []
