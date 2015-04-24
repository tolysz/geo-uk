{-# Language TemplateHaskell #-}

module Data.Geo.Type where

import Language.Haskell.TH.Lift
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.Internal as L8

instance Lift L8.ByteString where
  lift b = [| L8.pack $(lift $ L8.unpack b) |]
