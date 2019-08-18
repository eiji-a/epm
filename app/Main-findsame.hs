--
-- findsame
--
--   SQL: select id, fingerprint, status from images where id < 10000;
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
) where

import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
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
      findSame r0 r1 rt

findSame :: ColorUnit -> FpUnit -> Double -> IO ()
findSame r0 r1 rt = do
  ip <- T.getContents
  let
    ls = V.fromList $ T.lines ip
  hPutStrLn stderr ("STEP 1: get all lines/ " ++ show (V.length ls))
  cs <- V.forM ls $ \l -> do
    let
      img = T.splitOn "|" l
      i = read (T.unpack (img !! 0)) :: Int
      c = fp2colvec oreso vreso i (img !! 1) (img !! 2)
      b = isValid c      
    return (c, b)
  hPutStrLn stderr ("STEP 2: convert to image/ " ++ show (V.length cs))
  let
    (cvs, bools) = V.unzip cs
    nsim = V.length $ V.filter (== True) bools
  hPutStrLn stderr ("STEP 4: filter valid images/ " ++ show nsim)
  let
    cvmap = makeMap $ V.toList cvs
  hPutStrLn stderr ("STEP 5: make Kdt map/ " ++ show (getSize cvmap))
  putStrLn ("RES:" ++ show r0 ++ "," ++ show r1 ++ "," ++ show rt ++ "/"
            ++ (show $ V.length cvs) ++ "," ++ (show nsim))

  V.forM_ cs $ \(c, b) -> do
    if b == False
      then return ()
      else do
        let
          sim = findSimilarImage r0 r1 rt cvmap c
        if sim == []
          then return ()
          else putStrLn ((show $ colvec2id c) ++ ":" ++ (show (sort $ map colvec2id $ sim)))
  hPutStrLn stderr "STEP 6: finished!"

{-
  let
    sims = map (findSimilarImage r0 r1 rt cvmap) src
  putStrLn ("RES:" ++ show r0 ++ "," ++ show r1 ++ "," ++ show rt ++ "/" ++ (show $ length cvs) ++ "," ++ (show $ length src) ++ "," ++ (show $ length sims))

  forM_ (zip src sims) $ \s -> do
    let
      ids0 = map colvec2id $ snd s
    if ids0 /= []
      then putStrLn ((show $ colvec2id $ fst s) ++ ":" ++ (show ids0))
      else return ()
-}


