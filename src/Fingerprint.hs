-- Fingerprint
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fingerprint (
  Fingerprint
, FpUnit
, ColorUnit
, ColorVector
, fp2colvec
, makeMap
, findSimilarImage
, colvec2id
, colvec2status
, isValid
, getSize
) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import qualified Data.KdTree.Static as KT
import           Data.List (transpose)
import           Data.List.Split (chunksOf)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import           GHC.Generics
import           Numeric (readHex)
import           Numeric.LinearAlgebra hiding (R)

--type Fingerprint = String
type Fingerprint = T.Text
type FpUnit = Double  -- or Double

type ColorUnit = Double
type Channel = [ColorUnit]

type Color = [ColorUnit]  -- (r,g,b) => [r,g,b]
data ColorVector = ColorVector
  { pixel  :: [Color]
  , imgid  :: Int
  --, fp     :: [FpUnit]
  , fp     :: Fingerprint
  , status :: CvStatus
  } deriving (Eq, Show, Generic)
instance NFData ColorVector where
  rnf = genericRnf

type ColorVectorMap = KT.KdTree Double ColorVector

data CvStatus = FILED | PEND | DISCARDED | DUPLICATED | INFERIOR
  deriving (Eq, Show, Generic)
instance NFData CvStatus where
  rnf = genericRnf

-- constants
--   status

-- public functions

makeMap :: [ColorVector] -> ColorVectorMap
makeMap cvs = KT.buildWithDist colvec2points colvecDistance cvs

getSize :: ColorVectorMap -> Int
getSize cvmap = KT.size cvmap

{-|
  IN:
    r1  : radius for KdTree search
    r2  : difference for each pixel
-}

findSimilarImage :: ColorUnit -> FpUnit -> Double -> ColorVectorMap -> ColorVector
                 -> [ColorVector]
findSimilarImage r1 r2 rt cvmap cv = ss2
  where
    cvid = colvec2id cv
    funcNI = isNearImage r2 rt (colvec2fp cv)
    ss1 = filter (\s -> colvec2id s > cvid) $ KT.inRadius cvmap r1 cv
    ss2 = filter (\s -> funcNI (colvec2fp s)) ss1

{-|

>>> let fp1 = [1.0, 2.0, 1.0, 2.0]
>>> let fp2 = [1.0, 2.0, 1.0, 2.0]
>>> isNearImage 0.0 1.0 fp1 fp2
True
>>> let fp1 = [1.0, 2.0, 1.0, 2.0]
>>> let fp2 = [1.1, 2.0, 1.0, 2.0]
>>> let ds = map abs $ zipWith (-) fp1 fp2
>>> length ds
4
>>> let ds' = filter (<= 0.0) $ ds
>>> length ds'
3
>>> (fromIntegral (length ds') / fromIntegral (length ds))
0.75
>>> isNearImage 0.0 0.5 fp1 fp2
True

-}

isNearImage :: FpUnit -> Double -> [FpUnit] -> [FpUnit] -> Bool
isNearImage rad rate fp1 fp2 = (fromIntegral (length ds') / fromIntegral (length ds) >= rate)
  where
    ds = map abs $ zipWith (-) fp1 fp2
    ds' = filter (<= rad) ds

{-|
  IN:
      oreso - original resolution of x (ex. 8)
      vreso - ColorVector resolution of x (ex. 2)
      imgid - ID of image
      fp    - fingerprint of image (ex. 8 x 8 x 3ch)

>>> let fp = "000000010101020202030303010101020202030303040404020202030303040404050505030303040404050505060606"
>>> fp2colvec 4 2 1 fp "filed"
ColorVector {pixel = [[1.0,1.0,1.0],[3.0,3.0,3.0],[3.0,3.0,3.0],[5.0,5.0,5.0]], imgid = 1, fp = "000000010101020202030303010101020202030303040404020202030303040404050505030303040404050505060606", status = FILED}

-}

fp2colvec :: Int -> Int -> Int -> Fingerprint -> T.Text -> ColorVector
fp2colvec oreso vreso imgid fp st = ColorVector pixel imgid fp' st'
  where
    --fp' = fp2double fp
    fp' = fp
    d = oreso `div` vreso
    chs = map (matrix oreso) $ fp2channels fp
    pixel = transpose $ map (summary d vreso) chs
    --pixel = [[1,1,1],[3,3,3],[3,3,3],[5,5,5]]
    st' = case st of
      "filed"      -> FILED
      "pending"    -> PEND
      "deleted"    -> DISCARDED
      "duplicated" -> DUPLICATED
      "inferior"   -> INFERIOR
      _            -> DISCARDED

colvec2id :: ColorVector -> Int
colvec2id (ColorVector _ i _ _) = i

colvec2fp :: ColorVector -> [FpUnit]
colvec2fp (ColorVector _ _ fp _) = fp2double fp

colvec2status :: ColorVector -> CvStatus
colvec2status (ColorVector _ _ _ st) = st

isValid :: ColorVector -> Bool
isValid (ColorVector _ _ _ st) = (st == FILED || st == PEND)

-- internal functions

{-|

>>> let cv1 = ColorVector [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0], [10.0, 11.0, 12.0]] 1 "001122334455" FILED
>>> colvec2points cv1
[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0]

-}

colvec2points :: ColorVector -> [ColorUnit]
colvec2points (ColorVector points _ _ _) = concat points

{-|

>>> let cv1 = ColorVector [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0], [10.0, 11.0, 12.0]] 1 "001122334455" FILED
>>> let cv2 = ColorVector [[1.0, 1.0, 4.0], [3.0, 6.5, 6.0], [8.2, 8.0, 7.8], [10.0, 12.0, 11.0]] 2 "101122334455" FILED
>>> colvecDistance cv1 cv2
2.5

-}

colvecDistance :: ColorVector -> ColorVector -> Double
colvecDistance cv1 cv2 = maximum ls
  where
    p1s = colvec2points cv1
    p2s = colvec2points cv2
    ls = map sum $ chunksOf 3 $ zipWith (\x y -> abs (x - y)) p1s p2s

{-|

>>> let m = matrix 4 [0,1,2,3,1,2,3,4,2,3,4,5,3,4,5,6]
>>> summary 2 2 m
[1.0,3.0,3.0,5.0]
>>> let m2 = matrix 4 [6,5,4,3,5,4,3,2,4,3,2,1,3,2,1,0]
>>> summary 2 2 m2
[5.0,3.0,3.0,1.0]

-}

summary :: Int -> Int -> Matrix Double -> [Double]
summary d vreso m = map (\x -> x / fromIntegral (d*d)) col
  where
    ele = map (*d) [0..(vreso-1)]
    offset = [(x, y)| x <- ele, y <- ele]
    col = map (\o -> sum.toList.flatten $ subMatrix o (d, d) m) offset

{-|

>>> fp2channels "000102030405060708090a0b"
[[0.0,3.0,6.0,9.0],[1.0,4.0,7.0,10.0],[2.0,5.0,8.0,11.0]]

-}

fp2channels :: Fingerprint -> [Channel]
fp2channels fp = splitRgb ds ([], [], [])
  where
    ds = fp2double fp
    --ds = fp2int fp
    splitRgb :: [FpUnit] -> (Channel, Channel, Channel) -> [Channel]
    splitRgb [] (r, g, b) = [reverse r, reverse g, reverse b]
    splitRgb (r:g:b:xs) (rs, gs, bs) =
      splitRgb xs ((r:rs), (g:gs), (b:bs))

{-|

>>> fp2double "000102030405060708090a0b0c0d0e0f"
[0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
>>> fp2double "00102030405060708090a0b0c0d0e0f0"
[0.0,16.0,32.0,48.0,64.0,80.0,96.0,112.0,128.0,144.0,160.0,176.0,192.0,208.0,224.0,240.0]

-}

fp2double :: Fingerprint -> [Double]
fp2double fp = map hex2double $ T.chunksOf 2 fp

hex2double :: T.Text -> Double
hex2double hex = fromIntegral h
  where
    h0 = T.hexadecimal hex
    h = case h0 of
      Right (a, _) -> a
      Left _       -> 0
--    [(h, _)] = (readHex hex)
      
{-|

>>> fp2int "000102030405060708090a0b0c0d0e0f"
[0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
>>> fp2int "00102030405060708090a0b0c0d0e0f0"
[0.0,16.0,32.0,48.0,64.0,80.0,96.0,112.0,128.0,144.0,160.0,176.0,192.0,208.0,224.0,240.0]

-}

fp2int :: Fingerprint -> [Int]
fp2int fp = map hex2int $ T.chunksOf 2 fp

hex2int :: T.Text -> Int
hex2int hex = h
  where
    h0 = T.hexadecimal hex
    h = case h0 of
      Right (a, _) -> a
      Left _       -> 0




