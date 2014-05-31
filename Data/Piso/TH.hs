{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ParallelListComp #-}

module Data.Piso.TH (derivePisos) where

import Data.Piso
import Language.Haskell.TH
import Control.Applicative
import Control.Monad

-- | Derive partial isomorphisms for a given datatype. The resulting
-- expression is a tuple with one isomorphism element for each constructor in
-- the datatype.
-- 
-- For example:
-- 
-- > nothing :: Piso t (Maybe a :- t)
-- > just    :: Piso (a :- t) (Maybe a :- t)
-- > (nothing, just) = $(derivePisos ''Maybe)
-- 
-- Deriving isomorphisms this way requires @-XNoMonoPatBinds@.
derivePisos :: Name -> [String] -> Q [Dec]
derivePisos name pisoNames = do
  info <- reify name
  routers <-
    case info of
      TyConI (DataD _ _ tyArgs cons _)   ->
        mapM (derivePiso name tyArgs (length cons /= 1)) cons
      TyConI (NewtypeD _ _ tyArgs con _) ->
        (:[]) <$> derivePiso name tyArgs False con
      _ ->
        fail $ show name ++ " is not a datatype."
  return $ concat 
    [ [ SigD (mkName nm) typeF
      , ValD (VarP (mkName nm)) (NormalB router) []
      ] 
    | nm <- pisoNames 
    | (typeF, router) <- routers 
    ]

derivePiso :: Name -> [TyVarBndr] -> Bool -> Con -> Q (Type, Exp)
derivePiso resNm tyArgs matchWildcard con =
  case con of
    NormalC name tys -> go name (map snd tys)
    RecC name tys -> go name (map (\(_,_,ty) -> ty) tys)
    _ -> fail $ "Unsupported constructor " ++ show (conName con)
  where
    go name tys = do
      pisoE <- [| piso |]
      pisoCon <- deriveConstructor name tys
      pisoDes <- deriveDestructor matchWildcard name tys
      tNm <- newName "t"
      let t = VarT tNm
      let fromType = foldr (-:) t tys
      let toType = foldl (\t (PlainTV ty) -> AppT t (VarT ty)) (ConT resNm) tyArgs -: t
      return 
        $ ( ForallT (PlainTV tNm:tyArgs) [] $ ConT (mkName "Piso") `AppT` fromType `AppT` toType
          , pisoE `AppE` pisoCon `AppE` pisoDes
          )

(-:) :: Type -> Type -> Type
l -: r = ConT (mkName ":-") `AppT` l `AppT` r

deriveConstructor :: Name -> [Type] -> Q Exp
deriveConstructor name tys = do
  -- Introduce some names
  t          <- newName "t"
  fieldNames <- replicateM (length tys) (newName "a")

  -- Figure out the names of some constructors
  ConE cons  <- [| (:-) |]

  let pat = foldr (\f fs -> UInfixP (VarP f) cons fs) (VarP t) fieldNames
  let applyCon = foldl (\f x -> f `AppE` VarE x) (ConE name) fieldNames
  let body = UInfixE applyCon (ConE cons) (VarE t)

  return $ LamE [pat] body


deriveDestructor :: Bool -> Name -> [Type] -> Q Exp
deriveDestructor matchWildcard name tys = do
  -- Introduce some names
  r          <- newName "r"
  fieldNames <- replicateM (length tys) (newName "a")

  -- Figure out the names of some constructors
  ConE just  <- [| Just |]
  ConE cons  <- [| (:-) |]
  nothing    <- [| Nothing |]

  let conPat   = ConP name (map VarP fieldNames)
  let okBody   = ConE just `AppE`
                  foldr
                    (\h t -> UInfixE (VarE h) (ConE cons) t)
                    (VarE r)
                    fieldNames
  let okCase   = Match (UInfixP conPat cons (VarP r)) (NormalB okBody) []
  let failCase = Match WildP (NormalB nothing) []
  let allCases =
        if matchWildcard
              then [okCase, failCase]
              else [okCase]

  return $ LamCaseE allCases


-- Retrieve the name of a constructor.
conName :: Con -> Name
conName con =
  case con of
    NormalC name _  -> name
    RecC name _     -> name
    InfixC _ name _ -> name
    ForallC _ _ con' -> conName con'