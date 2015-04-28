
module Data.Geo.Conv
  ( UKOS(..)
  , WGS84 (..)
  , convWGS84toUKOS
  , convUKOStoWGS84
  , gridToGps
  , gpsToGrid
  , toUKOST
  , toUKOSTI
  )
where

import Data.Char (toUpper, toLower)
import Prelude
-- import System.Cmd
-- import System.IO.Unsafe
-- import System.IO.Capture
import Text.Printf

import Data.Geo.OSGM02Tab
import Data.Geo.Type
{-
capt p x = do 
          xa <- (capture (system (p++" "++x)) "")
          return $ fst xa
-}

-- WGS84  -- EPSG:4326
-- UKOS
-- OSGB36
-- ETRS89

data UKOS    = UKOST String
             -- ^ Coordinates as string
             | UKOSTI String
             -- ^ My altilery endoding for String 
             | UKOS Int Int
             -- ^ as Integers
             | UKOSD Double Double Double
             -- ^ With height
             | UKOSI Int Int Int

data WGS84   = WGS84  -- GPS
                  Double
                  Double
                  Double
                   deriving Show

data ETRS89  = ETRS89
                 Double
                 Double
                 Double
                   deriving Show
--type ETRS89 = WGS84EN

gpsToGrid la lo = ( round e , round n)
  where
    (UKOSD e n _ ) = convWGS84toUKOS ( WGS84 la lo 0 )

gridToGps e n = ( la, lo)
  where
   ( WGS84 la lo _ ) = convUKOStoWGS84 (UKOS e n )


instance Show UKOS where
 show (UKOS a b) = (show a)++" "++(show b)
 show (UKOST a) = a
 show (UKOSTI a) = a
 show (UKOSD e n h) = (show e)++" "++(show n) ++" "++ (show h)
 show (UKOSI e n h) = (td e)++" "++(td n) ++" "++ (td h)
                where
                  td x = printf "%.1f"(((fromIntegral x) * 0.1) :: Double)

toRad x = x * pi / 180
toDeg x = x * 180 / pi 


toPTPea (la,lo) = (ea,no)
              where
                (ETRS89 ea no _) = convWGS84toETRS89 $ WGS84 la lo 0

convWGS84toUKOS =  convETRS89toUKOS . convWGS84toETRS89 

r1 = round . (10 *)


diffETRS89toUKOS :: Double -> Double -> (Double, Double, Double)
diffETRS89toUKOS we wn 
      |     we < 0 
         || we > 700000 
         || wn < 0
         || wn > 1250000 = (we, wn, 0)
      | otherwise =  (se, sn, sg)
             where
               ei  = round (we / 1000)
               ni  = round (wn / 1000)
               (PTPDD se0 sn0 sg0 ) = gb ! (ei    , ni    )
               (PTPDD se1 sn1 sg1 ) = gb ! (ei + 1, ni    )
               (PTPDD se2 sn2 sg2 ) = gb ! (ei + 1, ni + 1)
               (PTPDD se3 sn3 sg3 ) = gb ! (ei    , ni + 1)
               dx = we - 1000 * (fromIntegral ei)
               dy = wn - 1000 * (fromIntegral ni)
               t = dx / 1000
               u = dy / 1000
               se = (1-t)*(1-u)*se0 + t*(1-u)*se1 + t*u*se2 + (1-t)* u * se3
               sn = (1-t)*(1-u)*sn0 + t*(1-u)*sn1 + t*u*sn2 + (1-t)* u * sn3
               sg = (1-t)*(1-u)*sg0 + t*(1-u)*sg1 + t*u*sg2 + (1-t)* u * sg3

convUKOStoETRS89 :: UKOS -> ETRS89
convUKOStoETRS89 a@(UKOST _)     = convUKOStoETRS89 $ normalizeUKOS a
convUKOStoETRS89 a@(UKOSTI _)    = convUKOStoETRS89 $ normalizeUKOS $ toUKOST a
convUKOStoETRS89 (UKOS a b)   = convUKOStoETRS89 $ UKOSI a b 0
convUKOStoETRS89 (UKOSI a b c) = convUKOStoETRS89 $ UKOSD (f a) (f b) (f c)
                     where
                      f = fromIntegral

convUKOStoETRS89 (UKOSD e n a) = ETRS89 (e - de) (n - dn) (a + da)
                     where
                       (de, dn, da) = loop 0 0
                       loop de1 dn1 = let
                                    h@(den, dnn, dan) = diffETRS89toUKOS (e - de1) (n - dn1)
                                    in
                                    case (abs(de1 - den) + abs(dn1 - dnn) < 0.000000001 ) of
                                      True -> h
                                      False -> loop den dnn


convETRS89toUKOS :: ETRS89 -> UKOS
convETRS89toUKOS (ETRS89 we wn wa) = UKOSD e n h -- UKOSI (r1 e,r1 n,r1 h)
             where
               (se, sn, sg) = diffETRS89toUKOS we wn
               e = we + se
               n = wn + sn
               h = wa - sg



convETRS89toWGS84 :: ETRS89 -> WGS84
convETRS89toWGS84 (ETRS89 ea no alt) = (WGS84 phi' lam' alt)
   where
    a = 6378137.000
    b = 6356752.31414
--    a = 6377563.396
--    b = 6356256.910
    f0 = 0.9996012717
    phi0 = toRad 49
    lam0 = toRad (-2)
    e0  = 400000 
    n0  = -100000

    de = ea - e0
    de2 = de * de

    a2 = a**2
    b2 = b**2
    e2 = (a2 - b2) / a2 -- B1

    phi = fixP ((no - n0) / a / f0 + phi0)

    fixP p = let d = (no - n0 -(calcM a b p))
                 r = d / a / f0 + p
             in
              if abs(d)<0.00001 then
                r
              else
                fixP r

    n   = (a - b)/(a+b)  -- B2
    n2 = n*n
    n3 = n2*n
    s = sin phi
    s2 = s*s

    omes = 1 / sqrt (1 - e2 *s2)

    ν  = a * f0 * omes --B3
    ρ = a * f0 * (1 - e2)*omes**3 --B4
    ν2 = 1 / (ν * ν)
    η = ν / ρ - 1 -- B5 nu²
--    phi' = phi
--    lam' = 0
    t = tan phi
    t2 = t * t
    vii = t / (2 * ρ * ν)

    viii' = vii * ν2 / 12
    viii = viii' * (5 + 3 * t2 * (1 - 3 * η) + η )

    ix' = viii' * ν2 / 30
    ix = ix' *(61 + 45 * t2 *(2 +  t2))
--    phi' = ix
    phi' =  toDeg $ phi - de2 * ( vii - de2 * (viii - de2 * ix))
    
    x = 1 / ( ν * (cos phi))
    xi' = x * ν2 / 6
    xi = xi' * (1 + η + 2 * t2)
    xii' = xi' * ν2 / 20
    xii = xii' * (5 + 4*t2*(7 + 6 * t2))
    xiia' = xii' * ν2 / 42
    xiia = xiia' * ( 61 + t2 *( 662 + t2*(1320 + 720 * t2)))
--    phi' = phi
    lam' = toDeg $ lam0 + de * (x - de2 * ( xi - de2 * (xii - de2 * xiia)))

convUKOStoWGS84 :: UKOS -> WGS84
convUKOStoWGS84  = convETRS89toWGS84 . convUKOStoETRS89


convWGS84toETRS89 :: WGS84 -> ETRS89
convWGS84toETRS89 (WGS84 phi' lam' alt) = ETRS89 ea no alt
  where
    phi = toRad phi'
    lam = toRad lam'
    a = 6378137.000
    b = 6356752.3141

    s = sin phi
    s2 = s*s

    c = cos phi
    c2 = c*c
    c3 = c2 * c
    c5 = c3 * c2

    ta2 = s2/c2
    ta4 = ta2 * ta2

    a2 = a**2
    b2 = b**2
    e2 = (a2 - b2) / a2 -- B1

    f0 = 0.9996012717
    phi0 = toRad 49
    lam0 = toRad (-2)
    e0  = 400000 
    n0  = -100000

    omes = 1 / sqrt (1 - e2 *s2)

    ν  = a * f0 * omes --B3
    ρ = a * f0 * (1 - e2)*omes**3 --B4

    η = ν / ρ - 1 -- B5 nu²

    m = calcM a b phi

    i    = m + n0
    ii   = ν/2 * s * c
    iii  = ν/24 * s * c3 * (5 - ta2 + 9*η)
    iiia = ν/720 * s * c5 * (61 -58 * ta2 + ta4)
    iv   = ν * c
    v    = ν/6 * c3 * (1 + η - ta2)
    vi   = ν/120 * c5 *(5 - 18 * ta2 + ta4 + η * (14 - 58 * ta2))

    mlam  = lam - lam0
    mlam2 = mlam * mlam

    ea = e0 + mlam * (iv + mlam2 * (v + mlam2 * vi))
    no = i +  mlam2 * (ii + mlam2 * (iii + iiia * mlam2))


calcM a b phi = m
        where
--            a = 6378137.000
--            b = 6356752.3141
--            a = 6377563.396
--            b = 6356256.910
            f0 = 0.9996012717
            phi0 = toRad 49
---            lam0 = toRad (-2)

            n   = (a - b)/(a+b)  -- B2
            n2 = n*n
            n3 = n2*n


            pphi = phi + phi0
            mphi = phi - phi0
            m1 = (1 + n + 5/4 * (n2+n3)) * mphi
            m2 = (3*(n+n2) + 2.625*n3) * (sin mphi) * (cos pphi)
            m3 = 1.875*(n2+n3) * (sin $ 2 * mphi) * (cos $ 2 * pphi)
            m4 = 35/24*n3 * (sin $ 3 * mphi) * (cos $ 3 * pphi)
            m =  f0 * b * (m1 - m2 + m3 - m4)



-- convUKOStoWGS84' :: UKOS -> WGS84
-- convUKOStoWGS84' u = WGS84 la lo a
--                     where
--                      (lo:la:a:as) = map (\x->(read x )::Double) $ words (unsafePerformIO $ capt "gridToGPS" (show u)) 


fZ a = reverse $ (take 5) $ (reverse (show $ a `mod` 100000)) ++ "00000"


toUKOSTI :: UKOS -> UKOS
toUKOSTI (UKOST (a1:b1:a2:a3:a4:a5:a6:b2:b3:b4:b5:b6:[])) = UKOSTI (toLower a1:toLower b1:a2:b2:a3:b3:a4:b4:a5:b5:a6:b6:[])
toUKOSTI a = toUKOSTI $ toUKOST a


toUKOST :: UKOS -> UKOS
toUKOST a@(UKOST _) = a
toUKOST (UKOSTI (a1:b1:a2:b2:a3:b3:a4:b4:a5:b5:a6:b6:[])) = (UKOST (toUpper a1:toUpper b1:a2:a3:a4:a5:a6:b2:b3:b4:b5:b6:[]))
toUKOST (UKOSTI a) = toUKOST $ UKOSTI $ fx $ ll (length a)
                         where
                           ll 0 = "NT55"
                           ll 1 = "T55"
                           ll a = if (even a) then "55" else "5"
                           fx b = take 12 (a ++ b ++ (repeat '0'))

toUKOST (UKOS  a b) = UKOST $ (toLetters a b)  ++ (fZ a) ++ (fZ  b) 
toUKOST (UKOSI a b c) = toUKOST $ UKOS a b
toUKOST (UKOSD a b c) = toUKOST $ UKOS (round a) (round b)

--- TG 5140 1317 can also be expressed as 65140,31317
toLetters :: Int -> Int -> String
toLetters e' n' = [x,y] --  ++ (show $ e'-e * 100000) ++ (show $ n'-n * 100000)
   where
     e = e' `div` 100000
     n = n' `div` 100000
     l1 = 19 - n - (19 - n) `mod`5 + (e+10) `div` 5
     l2 = ((19 -n)*5) `mod` 25 + e `mod` 5 
     l1f | l1 > 7    = 1
         | otherwise = 0
     l2f | l2 > 7    = 1
         | otherwise = 0
     x = toEnum (65 + l1 + l1f)
     y = toEnum (65 + l2 + l2f)


normalizeUKOS :: UKOS -> UKOS

normalizeUKOS (UKOST tt) = UKOS e' n'
             where
               (a:b:t) = filter (/=' ') tt
               ia  = (fromEnum $ toUpper a) - 65
               ia' = if (ia > 7) then (ia - 1) else ia
               ib  = (fromEnum $ toUpper b) - 65
               ib' = if (ib > 7) then (ib - 1) else ib
               e =  ((ia' - 2) `mod` 5) * 5  + (ib' `mod` 5)
               n =  (19 - (ia' `div` 5) * 5) - (ib' `div` 5)
               l = (length t) `div` 2
               c = case l of 
                    1->5000
                    2->500
                    3->50
                    4->5
                    _->0
               (e1,n1) = case l of 
                         0 -> (0,0)
                         _ -> ((read $ take l t)::Int,(read $ drop l t):: Int)
               e' = (10 ^ l * e + e1) * 10 ^ (5 -l) + c
               n' = (10 ^ l * n + n1) * 10 ^ (5 -l) + c
normalizeUKOS a@(UKOSTI _ ) = normalizeUKOS $ toUKOST a
normalizeUKOS a = a
