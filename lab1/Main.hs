module Main where

import Solution1
import System.Random
import Given
--main = print (stratJackknife (sum) [1..550])


main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Double] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  print (stratJackknife (sum) rs)


