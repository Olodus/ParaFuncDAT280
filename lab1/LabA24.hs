module LabA24 where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par
import Control.Monad
import Data.List

-- Given
--
-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int


mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1


resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500


-- Our Solution

-- Assignment 1
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
merge (xl@(x:xs), yl@(y:ys)) 
  | x <= y = (x:merge (xs, yl))
  | x >  y = (y:merge (xl, ys))

merge2 :: Ord a => [a] -> [a] -> [a]
merge2 [] y = y
merge2 x [] = x
merge2 (x:xs) (y:ys)  
    | x <= y = x : merge2 xs (y:ys)
    | x >  y = y : merge2 (x:xs) ys

-- With Par Monad

-- parMergesort
-- Forks/spawn for everytime mergesort splits the list. 
parMergesort :: (NFData a, Ord a) => [a] -> Int -> Par [a]
parMergesort x depth 
  | depth > 6 = return (mergesort x)
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

-- With Strategies

-- divConq
-- Solves a problem using divide and conquer
divConq :: (a -> b) 
        -> a
        -> (b -> b -> b)
        -> (a -> Maybe (a,a))
        -> b
divConq f arg combine divide = go arg 6 -- we have tried a bunch of different depths
    where
        go arg depth =
            case divide arg of
              Nothing   -> f arg
              Just (l0, r0) -> combine l1 r1 `using` strat
                where 
                    l1 = go l0 (depth-1)
                    r1 = go r0 (depth-1)
                    strat x = do r l1; r r1; return x
                       where r | depth >= 0 = rpar
                               | otherwise  = rseq

-- stratMerge
-- Solves mergesort using the divide and conquer skeleton.
stratMerge :: Ord a => [a] -> [a]
stratMerge l = divConq f l combine divide
    where 
        f   :: Ord a => [a] -> [a]
        f   x = x
        combine   :: Ord a => [a] -> [a] -> [a]
        combine x1 x2 = merge2 x1 x2
        divide    :: [a] -> Maybe([a],[a])
        divide x = case (splitAt (length x `div` 2) x) of
                     ([],x2) -> Nothing
                     (x1,[]) -> Nothing
                     res     -> Just res

