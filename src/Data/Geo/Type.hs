{-# Language TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

module Data.Geo.Type where

import Language.Haskell.TH
import Language.Haskell.TH.Lift
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.Internal as L8

import Data.Binary
import Data.Array.Unboxed as Ar

instance Lift L8.ByteString where
  lift b = [| L8.pack $(lift $ L8.unpack b) |]

data PTPDD = PTPDD !Double !Double !Double deriving Show

instance Lift PTPDD where
 lift (PTPDD a b c) = [| PTPDD $(lift a) $(lift b) $(lift c) |]

instance Binary PTPDD where
  put (PTPDD de dn da ) =
    put de >>
    put dn >>
    put da
  get = do
    de <- get
    dn <- get
    da <- get
    return (PTPDD de dn da )

type ArrDD = Ar.Array ((Int,Int)) PTPDD

instance Lift ArrDD where
  lift b = [|Ar.listArray $(lift $ Ar.bounds b) $(lift $ Ar.elems b)|]
