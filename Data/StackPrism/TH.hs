{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.StackPrism.TH (
    -- * Deriving stack prisms
    deriveStackPrisms, deriveStackPrismsWith, deriveStackPrismsFor,

    -- * Re-exported types from @Data.StackPrism@
    StackPrism, (:-)(..)
  ) where

import Data.StackPrism
import Language.Haskell.TH
import Control.Monad

-- | Derive stack prisms for a given datatype.
--
-- For example:
--
-- > deriveStackPrisms ''Maybe
--
-- will create
--
-- > _Just :: StackPrism (a :- t) (Maybe a :- t)
-- > _Nothing :: StackPrism t (Nothing :- t)
--
-- together with their implementations.
deriveStackPrisms :: Name -> Q [Dec]
deriveStackPrisms = deriveStackPrismsWith ('_':)

-- | Derive stack prisms given a function that derives variable names from constructor names.
deriveStackPrismsWith :: (String -> String) -> Name -> Q [Dec]
deriveStackPrismsWith nameFun = deriveStackPrismsWith' (const nameFun)

-- | Derive stack prisms given a list of variable names, one for each constructor.
deriveStackPrismsFor :: [String] -> Name -> Q [Dec]
deriveStackPrismsFor names = deriveStackPrismsWith' (\i _ -> names !! i)

deriveStackPrismsWith' :: (Int -> String -> String) -> Name -> Q [Dec]
deriveStackPrismsWith' nameFun name = do
  info <- reify name
  routers <-
    case info of
      TyConI (DataD _ _ tyArgs cons _)   ->
        mapM (deriveStackPrism name tyArgs (length cons /= 1)) cons
      TyConI (NewtypeD _ _ tyArgs con _) ->
        (:[]) <$> deriveStackPrism name tyArgs False con
      _ ->
        fail $ show name ++ " is not a datatype."
  return $ concat 
    [ [ SigD nm typeF
      , ValD (VarP nm) (NormalB router) []
      ] 
    | (i, (conNm, typeF, router)) <- zip [0..] routers
    , let nm = mkName (nameFun i (nameBase conNm))
    ]

deriveStackPrism :: Name -> [TyVarBndr] -> Bool -> Con -> Q (Name, Type, Exp)
deriveStackPrism resNm tyArgs matchWildcard con =
  case con of
    NormalC name tys -> go name (map snd tys)
    RecC name tys -> go name (map (\(_,_,ty) -> ty) tys)
    InfixC (_, tyl) name (_, tyr) -> go name [tyl, tyr]
    _ -> fail $ "Unsupported constructor " ++ show (conName con)
  where
    go name tys = do
      stackPrismE <- [| stackPrism |]
      stackPrismCon <- deriveConstructor name tys
      stackPrismDes <- deriveDestructor matchWildcard name tys
      tNm <- newName "t"
      let t = VarT tNm
      let fromType = foldr (-:) t tys
      let toType = foldl (\t' (PlainTV ty) -> AppT t' (VarT ty)) (ConT resNm) tyArgs -: t
      return 
        $ ( name
          , ForallT (PlainTV tNm:tyArgs) [] $ ConT (mkName "StackPrism") `AppT` fromType `AppT` toType
          , stackPrismE `AppE` stackPrismCon `AppE` stackPrismDes
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