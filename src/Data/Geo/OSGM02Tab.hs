{-# Language TemplateHaskell, CPP #-}
module Data.Geo.OSGM02Tab
 ( (!)
 , PTPDD(..)
 , gb
 ) where


import Data.Maybe
import Data.Geo.Type
import Data.Array.Unboxed ((!))

#if EMBEDED_DATA
import qualified Codec.Compression.BZip as BZip
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Binary (decode)
import Data.FileEmbed
import qualified Data.ByteString

gb :: Maybe ArrDD
gb = Just (decodePTPDDFile . L8.fromStrict $ $(embedFile "data/GB.dat"))

decodePTPDDFile :: L8.ByteString -> ArrDD
decodePTPDDFile = decode . BZip.decompress

#else
gb :: Maybe ArrDD
gb = Nothing
#endif

