{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Piso.TH (derivePisos, derivePisosWith, derivePisosFor) where

import Data.Piso
import Language.Haskell.TH
import Control.Applicative
import Control.Monad

-- | Derive partial isomorphisms for a given datatype.
--
-- For example:
--
-- > derivePisos ''Maybe
--
-- will create
--
-- > _Just :: Piso (a :- t) (Maybe a :- t)
-- > _Nothing :: Piso t (Nothing :- t)
--
-- together with their implementations
derivePisos :: Name -> Q [Dec]
derivePisos = derivePisosWith ('_':)

derivePisosWith :: (String -> String) -> Name -> Q [Dec]
derivePisosWith nameFun = derivePisosWith' (const nameFun)

derivePisosFor :: [String] -> Name -> Q [Dec]
derivePisosFor names = derivePisosWith' (\i _ -> names !! i)

derivePisosWith' :: (Int -> String -> String) -> Name -> Q [Dec]
derivePisosWith' nameFun name = do
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
    [ [ SigD nm typeF
      , ValD (VarP nm) (NormalB router) []
      ] 
    | (i, (conName, typeF, router)) <- zip [0..] routers
    , let nm = mkName (nameFun i (nameBase conName))
    ]

derivePiso :: Name -> [TyVarBndr] -> Bool -> Con -> Q (Name, Type, Exp)
derivePiso resNm tyArgs matchWildcard con =
  case con of
    NormalC name tys -> go name (map snd tys)
    RecC name tys -> go name (map (\(_,_,ty) -> ty) tys)
    InfixC (_, tyl) name (_, tyr) -> go name [tyl, tyr]
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
        $ ( name
          , ForallT (PlainTV tNm:tyArgs) [] $ ConT (mkName "Piso") `AppT` fromType `AppT` toType
          , pisoE `AppE` pisoCon `AppE` pisoDes
          )

(-:) :: Type -> Type -> Type
l -: r = ConT (mkName ":-") `AppT` l `AppT` r

deriveConstructor :: Name -> [Type] -> Q Exp
deriveConstructor name tys = do
  -- Introduce some names
  t          <- newName "t"
  fieldNames <- replicateM (length tys) (newName "a")

  let cons = mkName ":-"
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