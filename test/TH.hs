{-# LANGUAGE LambdaCase #-}

module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

firstConstrForType :: String -> ExpQ
firstConstrForType typeName = lookupTypeName typeName >>= \case
  Nothing -> fail $ "Type does not exist: " ++ typeName
  Just name -> reify name >>= \case
    TyConI (DataD _ _ _ _ cons _) -> firstConstr cons
    TyConI (NewtypeD _ _ _ _ con _) -> firstConstr [con]
    _ -> fail $ "Not a data type: " ++ typeName
  where
    firstConstr [] = fail $ "Data type has no constructors: " ++ typeName
    firstConstr (c:_) = lift . nameBase =<< case c of
      NormalC name _ -> pure name
      RecC name _ -> pure name
      _ -> fail $ "Weird constructor: " ++ typeName

explode :: String -> ExpQ
explode [] = fail "Cannot explode empty string"
explode xs = listE $ map (litE . stringL . (:[])) xs
