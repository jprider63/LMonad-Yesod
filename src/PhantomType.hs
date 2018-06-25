{-# LANGUAGE DeriveLift, TemplateHaskell #-}

module PhantomType where

import qualified Data.Char as Char
import qualified Data.List as List
import Language.Haskell.TH
import Language.Haskell.TH.Lift

newtype PhantomType a = PhantomType Type

phantomType :: String -> Q Exp
phantomType s = case words s of
    [] ->
        error $ "Could not make phantom type: " ++ s
    conT:rest ->
        if length conT <= 1 || Char.isLower (head conT) then
            error $ "Invalid type constructor `" ++ conT ++ "`"
        else
            let con = ConT $ mkName conT in
            lift $ List.foldl' (\acc typ -> AppT acc (ConT (mkName typ))) con rest

-- instance Lift Type where
--     lift = $(makeLift ''Type)
$(deriveLift ''TyLit)
$(deriveLift ''TyVarBndr)
$(deriveLift ''Type)
    
            
