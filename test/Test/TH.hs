{-# LANGUAGE TemplateHaskell #-}
module Test.TH where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Language.Haskell.TH
import Network.KNX.IP.Serialize

getSimpleInstancesOf :: Name -> Q [Type]
getSimpleInstancesOf typ = do
  ClassI _ instances <- reify typ
  let inst = concatMap collectSimple instances
  return inst

  where
    collectSimple (InstanceD _ [] (AppT _ ty) _) = [ty]
    collectSimple _ = []

unsafeRight :: Either String b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error x

checkInverses :: [Type] -> Q Exp
checkInverses tys = do
  listE (map mkTest tys)
  
  where
    mkTest ty = do
      let tyLitE = litE . stringL $ pprint ty
      [| ($tyLitE, inverseL (unsafeRight . runGet (get :: Get $(return ty))) (runPut . put)) |]

