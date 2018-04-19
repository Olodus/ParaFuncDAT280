module Solution1 where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par
import Given
import Control.Monad

-- a) Using par & pseq

-- myParMap
-- Executes map on a list in parallel. 
-- Recursively calls par and pseq on 
-- every element with the given function applied to it.
myParMap :: (a -> b) -> [a] -> [b]
myParMap f [] = []
myParMap f (l:ls) = par x (pseq y (x:y))
    where 
        x = f l
        y = myParMap f ls

-- parpseqJackknife 
-- Uses our "myParMap" to run jackknife in parallel
parpseqJackknife :: ([a] -> b) -> [a] -> [b]
parpseqJackknife f l = myParMap f $ resamples 500 l


-- b) Eval Monad

-- evalJackknife
-- Uses the Eval monad to run jackknife in parallel
evalJackknife :: ([a] -> b) -> [a] -> [b] 
evalJackknife f l = runEval (parMapEval f $ resamples 500 l)

-- Parallel map using the Eval monad.
parMapEval :: (a -> b) -> [a] -> Eval [b]
parMapEval _ [] = return []
parMapEval f (x:xs) = do
    y  <- rpar (f x)
    ys <- parMapEval f xs
    return (y:ys)


-- c) Strategies

-- stratJackknife
-- Uses Strategy to run jackknife in parallel
stratJackknife :: ([a] -> b) -> [a] -> [b]
stratJackknife f xs = map f (resamples 500 xs) `using` jackstrat rseq

-- evalstrat
-- Applies a strategy to every element in a list
-- Same as evalList but we wanted to 
-- implement it for better understanding. 
evalstrat :: Strategy a -> Strategy [a]
evalstrat strat [] = return []
evalstrat strat (x:xs) = do
    y <- strat x
    ys <- evalstrat strat xs
    return (y:ys)

-- jackstrat
-- Applies a strategy in parallel to every element in a list.
-- Same as parList but we wanted to implement it for better understanding.
jackstrat :: Strategy a -> Strategy [a]
jackstrat strat = evalstrat (rparWith strat)



-- d) Par Monad

-- myspawn
-- Forks some work and returns an IVar pointing to it.
-- Same as spawn but we wanted to implement it for better understanding. 
myspawn :: NFData a => Par a -> Par (IVar a)
myspawn p = do
    i <- new 
    fork (do x <- p; put i x)
    return i

-- myParMapM
-- Uses myspawn to apply a function to every element in a list.
-- (mostly) The same as parMapM but we wanted to implement it ourselves.
myParMapM :: NFData b => (a -> b) -> [a] -> Par [b]
myParMapM f xs = do
    ibs <- mapM (myspawn . return . f) xs
    mapM get ibs

-- monadJackknife
-- Uses myParMapM to run jackknife in parallel
monadJackknife :: NFData b => ([a] -> b) -> [a] -> [b]
monadJackknife f l = runPar $ myParMapM (f) (resamples 500 l)



-- Assignment 2
-- We were not able to solve this assignment. 
-- We hope you can help us understand where we go wrong. 

-- Mergesort without parallism

-- mergesort
-- Sorts a list of ordered items by recursivly splitting and merging.
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort x | l > 1 =  merge $ mapTuple (mergesort) (splitAt (quot l 2) x)
            | otherwise = x
            where l = length x

-- mapTuple
-- Help function to map a function on to a pair.
mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (a1,a2) = (f a1, f a2)

-- merge
-- Help function to mergesort.
-- Merges to lists into an ordered list. 
merge :: Ord a => ([a],[a]) -> [a]
merge ([], y) = y
merge (x, []) = x
merge ((x:xs), (y:ys)) 
  | x <= y = (x:merge (xs, (y:ys)))
  | x >  y = (y:merge ((x:xs), ys))


-- With Par Monad

-- parMergesort
-- Forks/spawn for everytime mergesort splits the list. 
parMergesort :: (NFData a, Ord a) => [a] -> Int -> Par [a]
parMergesort x depth 
  | depth > 8 = return (mergesort x)
  | l > 1 = do 
    i <- spawn (parMergesort x1 (depth+1))
    j <- spawn (parMergesort x2 (depth+1))
    (parmerge i j)
  | otherwise = do return x
    where l = length x
          (x1, x2) = splitAt (quot l 2) x


-- parmerge 
-- Waits on two IVar's with lists and then merges them
parmerge :: Ord a => IVar [a] -> IVar [a] -> Par [a]
parmerge a b = do
    a' <- get a
    b' <- get b
    return (merge (a',b'))

-- monadMerge
-- Sorts a list with mergesort using the Par Monad
monadMerge :: (NFData a, Ord a) => [a] -> [a]
monadMerge l = runPar $ parMergesort l 0


-- Eval Monad

-- mergesort2
-- Sorts a list with mergesort using the Eval Monad
mergesort2 :: Ord a => [a] -> [a]
mergesort2 x = runEval (evalMergesort 4 x)

-- evalMergesort
-- Splits, roar's and mergesorts a list. Returns a Eval list
-- (We know it isn't the most beautiful code but we had problems 
-- since it wasn't running in parallel on one of our systems
-- and we could not find out why. In the end we didn't have time
evalMergesort :: Ord a => Integer -> [a] -> Eval [a]
evalMergesort _ [] = return []
evalMergesort _ [x] = return [x]
evalMergesort t x | (t <= 0) = do
      let l = length x
      let split = (splitAt (quot l 2) x)
      let l1 = (fst split)
      let l2 = (snd split)
      let l1' = rseq (evalMergesort 0 l1)
      let l2' = rseq (evalMergesort 0 l2)
      l3 <- l1'
      l4 <- l2'
      l5 <- l3
      l6 <- l4
      return (merge (l5, l6))
                   | otherwise  = do 
      let l = length x
      let split = (splitAt (quot l 2) x)
      let l1 = (fst split)
      let l2 = (snd split)
      let l1' = rpar (evalMergesort (t-1) l1)
      let l2' = rpar (evalMergesort (t-1) l2)
      l3 <- l1'
      l4 <- l2'
      l5 <- l3
      l6 <- l4
      return (merge (l5, l6))

-- With Strategies

stratMerge :: Ord a => [a] -> [a]
stratMerge l = mergesort l `using` mergeStrat 15

mergeStrat :: Ord a =>  Integer -> Strategy [a]
mergeStrat threshold = evalMerge threshold

evalMerge :: Ord a =>  Integer -> Strategy [a]
evalMerge  _ [] = return []
evalMerge  _ [x] = return [x]
evalMerge t x | (t <= 0) = do -- after threshold
     let l = length x
     let split = (splitAt (quot l 2) x)
     let l1 = (fst split)
     let l2 = (snd split)
     let l1' = rseq (evalMerge 0 l1)
     let l2' = rseq (evalMerge 0 l2)
     l3 <- l1'
     l4 <- l2'
     l5 <- l3
     l6 <- l4
     return (merge (l5, l6))
                   | otherwise  = do  -- before threshold
     let l = length x
     let split = (splitAt (quot l 2) x)
     let l1 = (fst split)
     let l2 = (snd split)
     let l1' = rpar (evalMerge (t-1) l1)
     let l2' = rpar (evalMerge (t-1) l2)
     l3 <- l1'
     l4 <- l2'
     l5 <- l3
     l6 <- l4
     return (merge (l5, l6))

