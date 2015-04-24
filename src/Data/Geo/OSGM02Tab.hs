{-# Language TemplateHaskell #-}
module Data.Geo.OSGM02Tab
 ( (!)
 , PTPDD(..)
 , gb
 ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.Internal as L8

import qualified Codec.Compression.BZip as BZip
import qualified System.Environment as Env
import Data.Maybe

import Data.Binary
import Data.Array

import System.IO.Unsafe

import Language.Haskell.TH.InlineIO
import Language.Haskell.TH.Lift
import Data.Geo.Type


data PTPDD = PTPDD !Double !Double !Double deriving Show

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

type ArrDD = Array ((Int,Int)) PTPDD

gb :: ArrDD
gb = decodePTPDDFile  $$(inlineIOAction $ L8.readFile "data/GB.dat")

decodePTPDDFile :: L8.ByteString -> ArrDD
decodePTPDDFile = decode . BZip.decompress
