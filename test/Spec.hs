module Main (
  main
) where

import Fingerprint
import Test.HUnit ((~=?))

main :: IO ()
main = do
  captured <- Silently.capture_ Fingerprint.
