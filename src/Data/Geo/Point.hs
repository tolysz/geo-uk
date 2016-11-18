-- {-# LANGUAGE EmptyDataDecls #-}

module Data.Geo.Point
(
 convGPStoUKOS,
 convGPStoUKOS',
 convUKOStoGPS,
 convUKOStoSHORT,
 convGPStoSHORT,
 convGPStoSHORT',
 convWGS84ENtoUKOS,
 GPS(..),
 WGS84EN(..),
 UKOS(..),
 SHORT(..)
)

where

import Data.Geo.OSGM02Tab
import Text.Printf
import Data.List (intercalate)

data GPS = GPS
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
    deriving (Show, Eq)

data WGS84EN =  WGS84EN
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
    deriving (Show, Eq)

data UKOS = UKOS
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double
    deriving (Show, Eq)

data SHORT = SHORT String
    deriving (Show, Eq)

convGPStoUKOS   =  convWGS84ENtoUKOS   . convWGS84toWGS84EN
convGPStoUKOS'  =  convWGS84ENtoUKOS'  . convWGS84toWGS84EN
convGPStoUKOS'' =  convWGS84ENtoUKOS'' . convWGS84toWGS84EN

-- todo: implement
convUKOStoGPS (UKOS e n a) = GPS la lo al
  where
    la = e
    lo = n
    al  = a

toRad x = x * pi / 180.0

convWGS84toWGS84EN :: GPS -> WGS84EN
convWGS84toWGS84EN (GPS phi' lam' alt) = WGS84EN ea no alt
  where
    phi = toRad phi'
    lam = toRad lam'
    a = 6378137.005
    b = 6356752.3141

    s = sin phi
    s2 = s*s

    c = cos phi
    c2 = c*c
    c3 = c2 * c
    c5 = c3 * c2

    ta = tan phi
    ta2 = ta * ta
    ta4 = ta2 * ta2

    a2 = a*a
    b2 = b*b
    e2 = (a2 - b2) / a2 -- B1

    f0 = 0.9996012717
    phi0 = toRad 49
    lam0 = toRad (-2)
    e0  = 400000 
    n0  = -100000

    n   = (a - b)/(a + b)  -- B2
    n2 = n*n
    n3 = n2*n
    omes = 1 / sqrt (1 - e2 *s2)

    ν  = a * f0 * omes --B3
    ρ = a * f0 * (1 - e2)*omes**3 --B4

    η = ν / ρ - 1 -- B5 nu²

    pphi = phi + phi0
    mphi = phi - phi0

    mlam  = lam - lam0
    mlam2 = mlam * mlam

    m1 = (1 + n + 5/4 * (n2+n3)) * mphi
    m2 = (3*(n+n2) + 21*n3/8) * (sin mphi) * (cos pphi)
    m3 = 15*(n2+n3)/8 * (sin $ 2 * mphi) * (cos $ 2 * pphi)
    m4 = 35*n3/24 * (sin $ 3 * mphi) * (cos $ 3 * pphi)
    m = b * f0 * (m1 - m2 + m3 - m4)

    i    = m + n0
    ii   = ν/2 * s * c
    iii  = ν/24 * s * c3 * (5 - ta2 + 9*η)
    iiia = ν/720 * s * c5 * (61 -58 * ta2 + ta4)
    iv   = ν * c
    v    = ν/6 * c3 * (1 + η - ta2)
    vi   = ν/120 * c5 *(5 - 18 * ta2 + ta4 + η * (14 - 58 * ta2))

    ea = e0 + mlam * (iv + mlam2 * (v + mlam2 * vi))
    no = i +  mlam2 * (ii + mlam2 * (iii + iiia * mlam2))

convWGS84ENtoUKOS :: WGS84EN -> UKOS
convWGS84ENtoUKOS pt@(WGS84EN we wn wa) = UKOS  e n h
       where
         ei  = round (we / 1000)
         ni  = round (wn / 1000)
         (PTPDD se0 sn0 sg0 ) = gb ! (ei,ni)
         (PTPDD se1 sn1 sg1 ) = gb ! (ei+1,ni)
         (PTPDD se2 sn2 sg2 ) = gb ! (ei+1,ni+1)
         (PTPDD se3 sn3 sg3 ) = gb ! (ei,ni+1)
         dx = we - 1000 * fromIntegral ei
         dy = wn - 1000 * fromIntegral ni
         t = dx / 1000
         u = dy / 1000
         se = (1-t)*(1-u)*se0 + t*(1-u)*se1 + t*u*se2 + (1-t)* u * se3
         sn = (1-t)*(1-u)*sn0 + t*(1-u)*sn1 + t*u*sn2 + (1-t)* u * sn3
         sg = (1-t)*(1-u)*sg0 + t*(1-u)*sg1 + t*u*sg2 + (1-t)* u * sg3
         e = we + se
         n = wn + sn
         h = wa - sg

convGPStoSHORT = convUKOStoSHORT . convGPStoUKOS
convGPStoSHORT' = convUKOStoSHORT . convGPStoUKOS'

convWGS84ENtoUKOS' :: WGS84EN -> UKOS
convWGS84ENtoUKOS' (WGS84EN e n a) =  UKOS  we' wn' wa'
  where
    ax x = floor (x / 100000)
    er  = err (ax e) (ax n)
    we' = e + (er !! 0) + e * (er !! 1) + n * (er !! 2)
    wn' = n + (er !! 3) + e * (er !! 4) + n * (er !! 5)
    wa' = a - ( (er !! 6) + e * (er !! 7) + n * (er !! 8))

--  tX(m)     tY(m)    tZ(m)   s(ppm)   rX(sec)  rY(sec) rZ(sec)
--  -446.448 +125.157 -542.060 +20.4894 -0.1502  -0.2470 -0.8421


convWGS84ENtoUKOS'' :: WGS84EN -> UKOS
convWGS84ENtoUKOS'' (WGS84EN e n a) =  UKOS  e n a
{--                             where
                               we' = -446.448 + (1+s) * e -     z * n +     y * a
                               wn' =  125.157 +     z * e + (1+s) * n -     x * a
                               wa' = -542.06  -     y * e +     x * n + (1+s) * a
                               x   = toRad (-0.1502/3600)
                               y   = toRad (-0.2470/3600)
                               z   = toRad (-0.8421/3600)
                               s   = 20.4894e-6
                               -}

convUKOStoSHORT (UKOS e n a)  = SHORT $ le ++ printf "%05d%05d" e1 n1
                          where
                            (le,(e1,n1)) = toLerrersD e n

shortToChunks :: SHORT -> [String]
shortToChunks s@(SHORT a) = case a of
                     [] -> []
                     _ -> let (s1, c) = shortLower s in shortToChunks s1 ++ [c]
chunksToShort :: [String] -> SHORT
chunksToShort a = case a of 
                  [] -> SHORT []
                  _  ->  shortRise (chunksToShort $ init a,last a)

shortLower :: SHORT -> (SHORT,String)
shortLower (SHORT z) = case z of
           []      -> (SHORT [], [])
           c@[a,b] -> (SHORT [], c)
           (a:b:d) -> let (d1,d2) = splitAt (length d `div` 2 ) d
                    in (SHORT $ a:b:(init d1 ++ init d2), [last d1,last d2])
shortRise :: (SHORT,String) -> SHORT
shortRise (SHORT z, u@[b1,b2]) = SHORT $ case z of
                    [] -> u
                    [a1,a2] -> z ++ u
                    (a1:a2:d) -> let (d1,d2) = splitAt (length d `div` 2 ) d in [a1,a2] ++ d1 ++ [b1] ++ d2 ++ [b2]

toLerrersD :: Double -> Double -> (String,(Int, Int))
toLerrersD de dn = (toLetters e' n',(mx e', mx n'))
   where
     mx x = x `mod` 100000
     e' = floor de
     n' = floor dn


--useless
shortPath  :: SHORT -> String
shortPath = shortPath' 3 "/"
shortPath'  :: Int -> String -> SHORT -> String
shortPath' i s = intercalate s .  take i . shortToChunks


shortFname :: SHORT -> String
shortFname = shortFname' 3 ""

shortFname' :: Int -> String ->SHORT -> String
shortFname' i s = intercalate s . drop i . shortToChunks

toLetters :: Int -> Int -> String
toLetters e' n' = [x,y]
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

err :: Int -> Int -> [Double]
err e1 n1 
   | (0 <= e1) &&  (e1 <7)  && (0 <= n1) && (n1 < 13 ) = errA !! e1 !! n1
   | otherwise = err0

err0 :: [Double]
err0 = [0, 0, 0, 0, 0, 0, 0, 0, 0]

errA :: [[[Double]]]
errA =[
 [ {-SV-} [ {- e 0.05 -}   90.52288448777226,   1.771640609711337e-5,  -3.968110086314743e-6,
            {- n 0.09 -}  -81.97690215456299,   5.801671900387466e-6,   2.05498753504267e-5,
            {- a 0.16 -}   54.16086589064195,  -1.2374369508321812e-5,  1.044645549968443e-5]
 ,  {-SQ-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-SL-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-SF-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-SA-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-NV-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-NQ-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-NL-} [ {- e 0.07 -}  70.095146012244,     4.1456716326394134e-5,  1.8417653958302014e-5,
            {- n 0.10 -}  -89.29867445606818,  -1.2323801318365644e-5,  4.3428442095516294e-5,
            {- a 0.23 -}   59.842128906720504, -1.7994298269853753e-5, -1.692843937774849e-6]
 ,  {-NF-} [ {- e 0.16 -}  68.81828347804016,   3.288606362703209e-5,   2.082836324766275e-5,
            {- n 0.23 -}  -79.18896197114888,  -1.791560690279764e-5,   3.150848192073447e-5,
            {- a 0.78 -}   60.863838150630386,  1.4668877565416474e-6, -4.359583495770336e-6]
 ,  {-NA-} [ {- e 0.27 -}  70.28529651005954,   3.177814511743803e-5,   1.9235957243168484e-5,
            {- n 0.11 -}  -77.64993577536787,  -1.6203188530974077e-5,  2.977744811139999e-5,
            {- a 0.26 -}   62.092635156034625,  7.69705514705232e-6,   -6.271754509363818e-6]
 ,  {-HV-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-HQ-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-HL-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ],
 [ {-SW-} [ {- e 0.14 -}   90.80978169291281,   1.585112702510758e-5,  -6.170754032232413e-6,
            {- n 0.18 -}  -82.1760149542062,    8.75629970952081e-6,    1.6127589151541195e-5,
            {- a 0.40 -}   53.858884412077856, -5.953692441787083e-6,   3.6196230996748744e-6]
 ,  {-SR-} [ {- e 0.04 -}  90.00181603066986,   1.818983596098826e-5,  -7.295200346930594e-7,
            {- n 0.04 -}  -82.18658886355912,   1.0184389192110637e-5,  1.1372632783781237e-5,
            {- a 0.04 -}   53.69798602894435,  -8.092609058278251e-6,   9.674506152639708e-6]
 ,  {-SM-} [ {- e 0.10 -}  92.37718214621535,   1.7318833209380362e-5, -1.1778761404931993e-5,
            {- n 0.17 -}  -82.16028180325398,   1.2635936747809663e-6,  1.9173883823504635e-5,
            {- a 0.21 -}   53.78614318737266,   1.786016148407633e-6,   4.359722588554455e-7]
 ,  {-SG-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-SB-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-NW-} [ {- e 0.20 -}  81.30019018028854,   4.659770898496933e-5,   0.0,
            {- n 0.32 -}  -94.7245639947871,   -3.944696399927192e-6,   4.988850099894382e-5,
            {- a 0.15 -}   53.19491022027531,  -1.428250818795129e-6,   3.5212072388438312e-6]
 ,  {-NR-} [ {- e 0.29 -}  75.13310056061242,   4.584020128113958e-5,   1.0249220740376957e-5,
            {- n 0.24 -}  -91.83477053019777,  -9.453151514702293e-6,   4.6795538519403626e-5,
            {- a 0.34 -}   57.61508310451514,  -1.178981067351522e-5,  -4.893691707076139e-7]
 ,  {-NM-} [ {- e 0.26 -}  74.04052873630252,   4.0101239674190794e-5,  1.3336596983337878e-5,
            {- n 0.15 -}  -85.45094747585799,  -1.393304767204118e-5,   3.864029278114543e-5,
            {- a 0.47 -}   56.11161262592326,  -1.5419820246927864e-5,  2.377298433617407e-6]
 ,  {-NG-} [ {- e 0.23 -}  73.37964291092905,   3.426030974051923e-5,   1.5317234183553313e-5,
             {- n 0.21 -} -79.7121261802969,   -1.4677488465917154e-5,  3.171074965778426e-5,
            {- a 0.79 -}   55.72600789248651,  -1.3588535700944043e-5,  2.42660803532412e-6]
 ,  {-NB-} [ {- e 0.24 -}  69.96146415831528,   3.1871759090260525e-5,  1.9474489658586712e-5,
            {- n 0.17 -}  -77.20372589070988,  -1.8434860010365975e-5,  2.9550960965833617e-5,
            {- a 0.68 -}   60.43829049520493,  -2.6236250473743997e-5, -4.1384542225929953e-7]
 ,  {-HW-} [ {- e 0.11 -}  81.0075164766613,    2.498338254966023e-5,   9.78227636125698e-6,
            {- n 0.10 -}  -71.23766890513403,  -1.8900777300182064e-5,  2.3427687919944602e-5,
            {- a 0.06 -}   50.0661485225658,   -1.7515959279742176e-5,  8.336423438735306e-6]
 ,  {-HR-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-HM-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ],
 [ {-SX-} [ {- e 0.24 -}   92.39764517180544,   7.423980823396427e-6,  -3.6518087727609324e-6,
            {- n 0.36 -}  -80.5776547431408,    2.30277148198392e-6,    1.1216403530586227e-5,
            {- a 0.36 -}   55.8957689914599,   -1.702682286397354e-5,   9.713070709641694e-6]
 ,  {-SS-} [ {- e 0.22 -}  90.90455962480074,   1.177337446411876e-5,   1.015049346857707e-6,
            {- n 0.52 -}  -80.74490178597344,   2.5348561173139295e-6,  1.254434531143069e-5,
            {- a 0.53 -}   58.05622844562254,  -2.0408212838645195e-5, -5.6176159987819115e-8]
 ,  {-SN-} [ {- e 0.27 -}  90.97631986468642,   1.554592774338374e-5,  -4.224436922651896e-6,
            {- n 0.21 -}  -81.87456981945793,   4.42690832816036e-6,    1.592269166028013e-5,
            {- a 0.71 -}   53.6406628750235,   -1.1323388514945777e-5,  1.2782189874710822e-5]
 ,  {-SH-} [ {- e 0.17 -}  86.9396755509341,    2.5397229177078063e-5,  3.0863770029901185e-7,
            {- n 0.24 -}  -83.73394982971897,  -1.7730264033261606e-8,  2.6603535855641244e-5,
            {- a 1.05 -}   54.29821641646371,  -2.759963180823525e-6,   2.357765054092797e-6]
 ,  {-SC-} [ {- e 0.20 -}  85.94998665676047,   3.6717961439737056e-5, -3.878256412059031e-6,
            {- n 0.30 -}  -87.81151052108721,   4.903744002721596e-6,   3.386541613361297e-5,
            {- a 0.30 -}   59.43817011085465,  -2.7318805842807215e-5,  3.7127748033787525e-6]
 ,  {-NX-} [ {- e 0.34 -}  82.66368609997731,   4.381141287723362e-5,  -1.0957578820010838e-6,
            {- n 0.46 -}  -90.71286980050617,   1.8665212862866022e-6,  4.06107392897461e-5,
            {- a 0.61 -}   55.358434919279524, -1.6931290344007247e-5,  5.766061230401269e-6]
 ,  {-NS-} [ {- e 0.27 -}  78.84544249114778,   4.185631047443527e-5,   5.857129285156664e-6,
            {- n 0.25 -}  -89.74574556483091,  -6.4862155820504135e-6,  4.2573139933208596e-5,
            {- a 0.54 -}   61.62547676599221,  -1.3526483239504338e-5, -5.881556254931203e-6]
 ,  {-NN-} [ {- e 0.26 -}  77.5365941667659,    3.8096494476038534e-5,  9.117408035949307e-6,
            {- n 0.28 -}  -85.34974059824013,  -1.0212074477683081e-5,  3.756682304470693e-5,
            {- a 0.37 -}   58.389342423358386, -1.786609756381054e-5,   3.3042664062824906e-7]
 ,  {-NH-} [ {- e 0.36 -}  80.09277016290787,   3.186177811854415e-5,   7.90055430464681e-6,
            {- n 0.19 -}  -78.87269553025412,  -1.1933291866627447e-5,  2.9980867616708957e-5,
            {- a 0.89 -}   69.03918419206181,  -3.901001020327949e-5,  -6.4061951467131345e-6]
 ,  {-NC-} [ {- e 0.31 -}  78.43790304451116,   2.4274615164432352e-5,  1.1946014445323898e-5,
            {- n 0.23 -}  -72.71052331693787,  -1.2241326408873763e-5,  2.332365127717442e-5,
            {- a 1.29 -}   59.2821281304897,   -2.883689218275901e-5,   1.949440264314673e-6]
 ,  {-HX-} [ {- e 0.03 -}  78.29951980989975,   1.6962741132545425e-5,  1.3707093859872695e-5,
            {- n 0.06 -}  -73.02557169785176,  -1.4786051532648427e-5,  2.392312734278621e-5,
            {- a 0.04 -}   49.63719145541017,  -2.4428307464894576e-5,  1.0126312025153319e-5]
 ,  {-HS-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-HN-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ],
 [ {-SY-} [ {- e 0.20 -}   90.35317355327912,   1.2704908143067205e-5,  3.105659343849947e-6,
            {- n 0.32 -}  -78.68158990495004,  -5.792808495617704e-6,   1.8303006648247993e-5,
            {- a 0.15 -}   61.752741157868805, -3.7777208073851613e-5,  1.0925286801440674e-5]
 ,  {-ST-} [ {- e 0.27 -}  90.00149269655226,   1.3670194059199248e-5,  3.713153058314964e-6,
            {- n 0.29 -}  -79.85391866168851,  -1.701463136429657e-6,   1.5240029123710711e-5,
            {- a 0.36 -}   61.895442388945554, -3.681591173084471e-5,   5.975331279914501e-6]
 ,  {-SO-} [ {- e 0.34 -}  89.2578027524191,    1.8195939466778786e-5, -4.838643255157007e-7,
            {- n 0.25 -}  -78.32076991057853,  -6.206811085376694e-6,   1.4532146568886328e-5,
            {- a 0.39 -}   62.169802693133775, -4.2315799941853654e-5,  1.54839073562101e-5]
 ,  {-SJ-} [ {- e 0.35 -}  85.06948725234822,   2.6064539401184163e-5,  4.527615721708321e-6,
            {- n 0.26 -}  -82.08827962328652,  -4.870556107921036e-6,   2.6108406019949383e-5,
            {- a 0.66 -}   64.09507468909399,  -3.424806780908471e-5,  -1.6789935630711068e-7]
 ,  {-SD-} [ {- e 0.31 -}  81.28028239786372,   3.300617967643921e-5,   7.348412287682694e-6,
            {- n 0.26 -}  -85.84996064785939,  -6.805111153683477e-6,   3.722401534627942e-5,
            {- a 0.37 -}   56.17003757965627,  -1.8124119777288986e-5,  5.080613401741822e-6]
 ,  {-NY-} [ {- e 0.32 -}  82.9099838344486,    3.4297457746834334e-5,  3.4960467374395183e-6,
            {- n 0.20 -}  -85.81835551847428,  -4.37905577954339e-6,    3.529496642716745e-5,
            {- a 0.79 -}   63.002321307764625, -2.570516605784577e-5,  -3.0325899928473143e-6]
 ,  {-NT-} [ {- e 0.23 -}  81.46530543369387,   3.707708201450455e-5,   4.169011895242065e-6,
            {- n 0.21 -}  -88.49270033330029,  -3.129696208903974e-6,   3.915697745810547e-5,
            {- a 0.59 -}   74.72513604549255,  -3.856829499431298e-5,  -1.4471812929689018e-5]
 ,  {-NO-} [ {- e 0.16 -}  82.27391934810875,   3.5714229680953785e-5,  3.7240935547361663e-6,
            {- n 0.18 -}  -85.86619111491083,  -2.682517313190155e-6,   3.52644972952867e-5,
            {- a 0.38 -}   62.32470831660921,  -3.771426080043635e-5,   2.8600750565790936e-6]
 ,  {-NJ-} [ {- e 0.44 -}  84.60903583071052,   3.2839638191297425e-5,  2.018329402439795e-6,
            {- n 0.25 -}  -82.62870341306396,  -2.796801422518569e-6,   3.1295177719886155e-5,
            {- a 0.74 -}   76.01983328674949,  -2.5602698326002835e-5, -1.9374695403849523e-5]
 ,  {-ND-} [ {- e 0.14 -}  85.20039418362713,   2.4029478317209764e-5,  4.996353511286903e-6,
            {- n 0.30 -}  -74.28043777273933,  -7.818479246653993e-6,   2.3712597405589322e-5,
            {- a 0.47 -}   45.455656554034164, -4.1959646320039286e-5,  2.0452481385938432e-5]
 ,  {-HY-} [ {- e 0.16 -}  82.41589591048569,   2.1511457902804993e-5,  8.565793336245786e-6,
            {- n 0.10 -}  -71.64832242323922,  -9.836377433208938e-6,   2.154315671369795e-5,
            {- a 0.26 -}   58.701889059987415, -3.647036233158439e-5,   5.265165526931888e-6]
 ,  {-HT-} [ {- e 0.03 -}  76.30048363304455,   1.9102372502650146e-5,  1.5120258497490566e-5,
            {- n 0.08 -}  -69.43585850889417,  -9.021163687361717e-6,   1.898827398933772e-5,
            {- a 0.07 -}   39.60398567656571,  -4.2390475444205e-5,     2.4537599004769095e-5]
 ,  {-HO-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ],
 [ {-SZ-} [ {- e 0.27 -}   88.85177275787538,   1.8094513392797435e-5, -1.4020031075209261e-6,
            {- n 0.16 -}  -79.7888490443949,   -3.447193087521146e-6,   2.1428426756671514e-5,
            {- a 0.26 -}   57.21838040408761,  -2.4557170397354103e-5, -8.204376030093482e-8]
 ,  {-SU-} [ {- e 0.41 -}  88.31914561450563,   1.888827864410973e-5,   5.224150774006427e-7,
            {- n 0.29 -}  -77.73093191438194,  -6.214153267027287e-6,   1.2823260155123866e-5,
            {- a 0.36 -}   54.907613644439635, -2.262633827981173e-5,   1.3573994502786007e-5]
 ,  {-SP-} [ {- e 0.31 -}  85.94631756499462,   1.9536632829908817e-5,  1.0384893313008e-5,
            {- n 0.36 -}  -78.94065724422617,  -4.033965816776149e-6,   1.3205075694041516e-5,
            {- a 0.38 -}   55.93677742885333,  -2.407874626326553e-5,   1.2019213880813181e-5]
 ,  {-SK-} [ {- e 0.32 -}  82.0986633983993,    2.799652532600653e-5,   1.0715992113828116e-5,
            {- n 0.34 -}  -80.69168494584132,  -9.416958620665347e-6,   2.723571912673753e-5,
            {- a 0.52 -}   66.91185067542972,  -4.0249456323601674e-5,  0.0]
 ,  {-SE-} [ {- e 0.24 -}  79.86436625075939,   3.341134942188321e-5,   1.0178265660543577e-5,
            {- n 0.26 -}  -83.29531414584108,  -1.1116567437750087e-5,  3.5626362700159444e-5,
            {- a 0.45 -}   67.88224241653344,  -4.55048372549933e-5,    2.516095104336252e-6]
 ,  {-NZ-} [ {- e 0.19 -}  80.13266307306588,   3.640272341536937e-5,   7.027961942299519e-6,
            {- n 0.24 -}  -84.70482277449305,  -9.31227042540433e-6,    3.6831670823648956e-5,
            {- a 0.26 -}   71.65146415367695,  -4.2868230425791414e-5, -7.246410121698444e-6]
 ,  {-NU-} [ {- e 0.26 -}  75.63573278039513,   4.823042954169647e-5,   6.31709907441307e-6,
            {- n 0.18 -}  -91.7186163395619,   -1.0191542024246725e-6,  4.2907250364778254e-5,
            {- a 0.18 -}   75.34733079870044,  -4.464398234977953e-5,  -1.189086906958815e-5]
 ,  {-NP-} [ {- e 0.00 -}  84.90863049484831,   3.56267114628597e-5,    4.961683801898556e-7,
            {- n 0.01 -}  -84.9626044069825,   -7.294182994480272e-6,   3.6622611862107194e-5,
            {- a 0.02 -}   56.735906474460165, -3.167946936585657e-5,   7.0254618975708215e-6]
 ,  {-NK-} [ {- e 0.23 -}  88.21630393680489,   2.4679951978734546e-5,  1.624740078949503e-6,
            {- n 0.07 -}  -74.87048018268366,  -1.1452418394708153e-5,  2.6145178474170587e-5,
            {- a 0.33 -}   66.69564291242142,  -2.6835103741819795e-5, -7.73834501477183e-6]
 ,  {-NE-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-HZ-} [ {- e 0.10 -}  80.04567417074396,   2.397736529400148e-5,   9.715829842548488e-6,
            {- n 0.03 -}  -68.48306600511314,  -1.3659375972997263e-5,  2.0010267938563734e-5,
            {- a 0.20 -}   59.92684408346977,  -2.5301318849905616e-5, -2.168169480139904e-7]
 ,  {-HU-} [ {- e 0.21 -}  80.93147804613,      1.7287795660333398e-5,  1.1677377431185472e-5,
            {- n 0.23 -}  -63.43928040847214,  -1.2987275955790345e-5,  1.5199102585516218e-5,
            {- a 0.42 -}   50.254262251379515, -2.931283790964223e-5,   1.043825755937942e-5]
 ,  {-HP-} [ {- e 0.09 -}  89.2385004842188,    2.3377415013296095e-5,  2.4785737078386074e-6,
            {- n 0.07 -}  -62.763873469747374, -1.9453998774620844e-5,  1.7047725921145658e-5,
            {- a 0.24 -}   48.849286484094414, -2.2201572370244662e-5,  8.917824930947033e-6]
 ],
 [ {-TV-} [ {- e 0.07 -}   86.66470807189796,   2.0515803556248348e-5,  6.523471851465916e-6,
            {- n 0.09 -}  -76.4039203997246,   -1.2686984628139962e-5,  3.284067935192416e-5,
            {- a 0.09 -}   48.53791002501465,  -8.979386557197854e-6,   1.1208853796289746e-5]
 ,  {-TQ-} [ {- e 0.31 -}  85.81495046846861,   2.202492149088745e-5,   5.9922357157366e-6,
            {- n 0.38 -}  -77.47861894038168,  -7.725921648955388e-6,   1.695539153232851e-5,
            {- a 0.44 -}   50.21232220052862,  -1.1123262974424121e-5,  6.9705160930998875e-6]
 ,  {-TL-} [ {- e 0.37 -}  84.7982598433178,    2.2415604505701594e-5,  9.124113319931895e-6,
            {- n 0.29 -}  -76.7676261101454,   -7.428637207646123e-6,   1.1740513231047435e-5,
            {- a 0.25 -}   53.930324269587516, -1.6606524927864776e-5,  4.444828825655254e-6]
 ,  {-TF-} [ {- e 0.38 -}  81.69916708441713,   2.347748814720781e-5,   1.8333179970227718e-5,
            {- n 0.37 -}  -77.86464173791676,  -1.0098519864116955e-5,  2.013975549874628e-5,
            {- a 0.30 -}   60.810696868043905, -2.301373419866302e-5,  -7.077116021130554e-6]
 ,  {-TA-} [ {- e 0.18 -}  79.2151374787892,    3.2173466510989004e-5,  1.2951902216070757e-5,
            {- n 0.29 -}  -82.06523623539015,  -1.2776848902753259e-5,  3.491523668761615e-5,
            {- a 0.35 -}   62.994106625323596, -3.324712239145561e-5,   9.631009401376125e-8]
 ,  {-OV-} [ {- e 0.06 -}  80.94658429487306,   3.267620419956609e-5,   9.054120543269112e-6,
            {- n 0.03 -}  -88.61671784677661,   6.349649804987507e-7,   3.484439082691789e-5,
            {- a 0.02 -}   72.51156189998233,  -4.365907517899934e-5,  -7.87287279013998e-6]
 ,  {-OQ-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-OL-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-OF-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-OA-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-JV-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-JQ-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-JL-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ],
 [ {-TW-} [ {- e 0.00 -}   86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-TR-} [ {- e 0.25 -}  76.56285247406063,   3.61831838345325e-5,    1.0471821574905364e-5,
            {- n 0.25 -}  -70.29171802626188,  -2.0565213344287302e-5,  1.955097037029283e-5,
            {- a 0.10 -}   51.532083418574274, -1.2279032514631769e-5,  3.6707978672736565e-6]
 ,  {-TM-} [ {- e 0.32 -}  81.90318809661393,   2.740528041816393e-5,   8.926249058328857e-6,
            {- n 0.23 -}  -73.08347363597598,  -1.3578999849758215e-5,  1.156630571478055e-5,
            {- a 0.17 -}   53.223727081384915, -1.4816384988067206e-5,  3.2717770358746627e-6]
 ,  {-TG-} [ {- e 0.21 -}  80.20493010551203,   2.772430911253849e-5,   1.4550643248488113e-5,
            {- n 0.19 -}  -76.77323732783381,  -1.0363860778991768e-5,  1.7155860240304975e-5,
            {- a 0.03 -}   57.644478513458544, -1.5755872202636064e-5, -1.0041332571066183e-5]
 ,  {-TB-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-OW-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-OR-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-OM-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-OG-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-OB-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-JW-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-JR-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ,  {-JM-} [ {- e 0.00 -}  86.54657687332934,   2.603109777844444e-5,   5.795587861275795e-7,
            {- n 0.00 -}  -80.14015884792757,  -9.090888644603613e-6,   2.875016586001196e-5,
            {- a 0.00 -}   58.466053101983796,  2.385006156309881e-5,   1.8778836169588056e-6]
 ]]


tst = do
  print $ convGPStoUKOS' (GPS 51.543851 (-0.4709196) 0)
  print $ convGPStoUKOS  (GPS 51.543851 (-0.4709196) 0)
  print $ convWGS84ENtoUKOS $ convWGS84toWGS84EN (GPS 51.543851 (-0.4709196) 0)
  putStrLn "Oracle: 506131.248 183880.278 -46.098"
  print $ convWGS84ENtoUKOS' $ WGS84EN 506132.694 183880.496 0
  print $ convWGS84ENtoUKOS  $ WGS84EN 506132.694 183880.4960 0
