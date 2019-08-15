--
-- findsame
--
--   SQL: select id, fingerprint, status from images where id < 10000;
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Main (
  main
) where

import Control.Monad
import Data.List.Split
import System.Environment

import Fingerprint

oreso :: Int
oreso = 8
vreso :: Int
vreso = 2
radius0 :: Double
radius0 = 10.0

main :: IO ()
main = do
  as <- getArgs
  if length as /= 3
    then error "Usage: findsame <rad0> <rad1> <rate>"
    else do
      let
        r0 = read (as !! 0) :: Double
        r1 = read (as !! 1) :: Double
        rt = read (as !! 2) :: Double
      find r0 r1 rt

find :: Double -> Double -> Double -> IO ()
find r0 r1 rt = do
  c <- getContents
  let
    images = map (splitOn "|") $ lines c
    cvs =   map (\(i:fp:st:_) -> fp2colvec oreso vreso (read i ::Int) fp st) images
    cvmap = makeMap cvs
    src = filter isValid cvs
    sims = map (findSimilarImage r0 r1 rt cvmap) src

  putStrLn ("RES:" ++ show r0 ++ "," ++ show r1 ++ "," ++ show rt ++ "/" ++ (show $ length cvs) ++ "," ++ (show $ length src) ++ "," ++ (show $ length sims))
  forM_ (zip src sims) $ \s -> do
    let
      ids0 = map colvec2id $ snd s
    if ids0 /= []
      then putStrLn ((show $ colvec2id $ fst s) ++ ":" ++ (show ids0))
      else return ()



