module Network.KNX.IP.SerializeSpec where

import Test.Import
import Test.TH
import Language.Haskell.TH


inverses :: [(String, Property)]
inverses = $(getSimpleInstancesOf ''Serialize >>= checkInverses)
           ++ $(checkInverses
                 [ AppT (ConT ''ConnectRequest) (PromotedT 'DeviceMgmtConn)
                 , AppT (ConT ''ConnectResponse) (PromotedT 'DeviceMgmtConn)
                 , AppT (ConT ''ConnectRequest) (PromotedT 'TunnelConn)
                 , AppT (ConT ''ConnectResponse) (PromotedT 'TunnelConn)
                 ])
           

spec :: Spec
spec = describe "Serialize" $ do
  mapM_ inv inverses

  where
    inv (name, prop) = describe name $ describe "get" $ do
      it "is a left inverse to put" prop

