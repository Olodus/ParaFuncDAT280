module Main where

import Solution1
import System.Random
import Given
import Criterion.Main


main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Double] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  --print $ length (mergesort rs)
  

  defaultMain [
    bgroup "mergesort" [ bench "sequential" $ whnf length (mergesort rs)
                       , bench "Par Monad"  $ whnf length (monadMerge rs)
                       , bench "Eval Monad" $ whnf length (mergesort2 rs)
                       , bench "Strategies" $ whnf length (stratMerge rs)
                       ]
    ]

