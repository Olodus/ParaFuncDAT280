module Main where

import LabA24
import System.Random
import Criterion.Main



main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Double] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  --print "Seq \n"
  --print $ mergesort rs
  --print "Strat \n"
  --print $ stratMerge2 rs

  defaultMain [
    bgroup "mergesort" [-- bench "Par Monad"  $ nf (monadMerge) rs
                       --, bench "Sequential" $ nf (mergesort) rs
                          bench "Strategies" $ nf (stratMerge3) rs
                       ]
    ]

