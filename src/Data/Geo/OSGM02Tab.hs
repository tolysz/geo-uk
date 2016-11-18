{-# Language TemplateHaskell, CPP #-}
module Data.Geo.OSGM02Tab
 ( (!)
 , PTPDD(..)
 , gb
 ) where


import Data.Maybe
import Data.Geo.Type
import Data.Array.Unboxed ((!))
import qualified Codec.Compression.BZip as BZip
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Binary (decode)
-- import qualified Data.ByteString

#if EMBEDED_DATA
import Data.FileEmbed

gb :: ArrDD
gb = decodePTPDDFile . L8.fromStrict $ $(embedFile "data/GB.dat")
#else
import System.IO.Unsafe
import Paths_geo_uk

{-# NOINLINE gb #-}
gb :: ArrDD
gb = unsafePerformIO $ do
  fn <- getDataFileName "data/GB.dat"
  decodePTPDDFile <$> L8.readFile fn
#endif

decodePTPDDFile :: L8.ByteString -> ArrDD
decodePTPDDFile = decode . BZip.decompress
