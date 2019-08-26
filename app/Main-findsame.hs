--
-- findsame
--
--   SQL: select id, fingerprint, status from images where id < 10000;
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


module Main (
  main
) where

import Control.Monad
import Control.DeepSeq
import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Time as TM
import System.Environment
import System.IO

import Fingerprint

oreso :: Int
oreso = 8
vreso :: Int
vreso = 2
radius0 :: Double
radius0 = 10.0

main :: IO ()
main = do
  hSetBuffering stdout  NoBuffering
  as <- getArgs
  if length as /= 3
    then error "Usage: findsame <rad0> <rad1> <rate>"
    else do
      let
        r0 = read (as !! 0) :: ColorUnit
        r1 = read (as !! 1) :: FpUnit
        rt = read (as !! 2) :: Double
      ct0 <- TM.getZonedTime
      hPutStrLn stderr ("STEP 0 (" ++ show ct0 ++ "): start.")
      --cs <- readImage' V.empty
      cs <- makeImages
      ct2 <- TM.getZonedTime
      hPutStrLn stderr ("STEP 2 (" ++ show ct2 ++ "): convert to image/ " ++ show (V.length cs))
      let
        (cvs, _) = V.unzip cs
        cvmap = cvs `deepseq` makeMap $ V.toList $ V.filter (/= colorvector0) cvs
        mapsz = cvmap `deepseq` getSize cvmap
      ct5 <- TM.getZonedTime
      hPutStrLn stderr ("STEP 5 (" ++ show ct5 ++"): make Kdt map/ " ++ show mapsz)
      compImages r0 r1 rt cvmap cs
      ct3 <- TM.getZonedTime
      hPutStrLn stderr ("STEP 6: (" ++ show ct3 ++ "): finished the comparizon!")

readImage' :: V.Vector (ColorVector, Bool) -> IO (V.Vector (ColorVector, Bool))
readImage' !cs = do
  l <- T.getLine
  let
    img = T.splitOn "|" l
    i = read (T.unpack (img !! 0)) :: Int
    c = fp2colvec oreso vreso i (img !! 1) (img !! 2)
    b = isValid c
    cs' = cs V.++ V.singleton (c, b)
  eof <- isEOF
  cs' `deepseq` if eof
    then return cs'
    else readImage' cs'


makeImages :: IO (V.Vector (ColorVector, Bool))
makeImages = do
  ip <- T.getContents
  let
    ls = V.fromList $ T.lines ip
  ct1 <- TM.getZonedTime
  hPutStrLn stderr ("STEP 1 (" ++ show ct1 ++ "): get all lines/ " ++ show (V.length ls))
  let
    mkfpfunc = fp2colvec oreso vreso
  cs <- V.forM ls $ \l -> do
    let
      img = T.splitOn "|" l
      i = read (T.unpack (img !! 0)) :: Int
      c = mkfpfunc i (img !! 1) (img !! 2)
    --hPutStrLn stderr ("DEBUG: " ++ show i)
    c `deepseq` return (c, isValid c)
  return cs

compImages :: ColorUnit -> FpUnit -> Double -> ColorVectorMap
           -> V.Vector (ColorVector, Bool) -> IO ()
compImages r0 r1 rt cvmap cs = do
  putStrLn ("RES:" ++ show r0 ++ "," ++ show r1 ++ "," ++ show rt ++ "/"
            ++ (show $ V.length cs))
  cs' = filter (\(_, b) -> b) cs
  cs' `deepseq` V.forM_ cs' $ \(c, _) -> do
    let
      sim = findSimilarImage r0 r1 rt cvmap c
    if sim == []
      then return ()
      else putStrLn ((show $ colvec2id c) ++ ":" ++ (show (sort $ map colvec2id $ sim)))

