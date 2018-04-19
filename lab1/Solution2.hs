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


-- Sequential mergesort using Merger data type

-- Data type describing a mergesort tree
data Merger a
    = Split (Merger a,Merger a)
    | Ordered [a] 
    | End a

seqsplit :: [a] -> ([a],[a])
seqsplit xs = splitAt (quot l 2) xs
    where l = length xs


fullysplit :: [a] -> Merger a
fullysplit [] = error "Can't split empty list"
fullysplit xs | l > 1 = Split (fullysplit (fst s), fullysplit (snd s))
            | otherwise = End (head xs)
              where s = seqsplit xs
                    l = length xs

ms :: Ord a => Merger a -> Merger a -> Merger a
ms (Split (x,y)) a = ms (ms x y) a
ms a (Split (x,y)) = ms (ms x y) a
ms (End x) a = ms (Ordered [x]) a
ms a (End x) = ms (Ordered [x]) a
ms (Ordered xs) (Ordered ys) = Ordered (merge (xs, ys))

mergermerge :: Ord a => Merger a -> [a]
mergermerge (End x) = [x]
mergermerge (Ordered xs) = xs
mergermerge (Split (x, y)) = mergermerge (ms x y)

-- Sequential mergesort using Mergers
mergersort :: Ord a => [a] -> [a]
mergersort xs = mergermerge $ fullysplit xs

-- Par monad using Merger
depthLimit = 4

parMergerSort :: (NFData a, Ord a) => [a] -> Int -> Par [a]
parMergerSort xs depth = parMerger depth $ runPar $ parSplitter depth xs

parSplitter :: Int -> [a] -> Par (Merger a)
parSplitter _ [] = error "Can't split empty list"
parSplitter depth xs 
  | depth >= depthLimit = return (fullysplit xs)
  | length xs > 1 = do
      i <- myspawn (parSplitter (depth+1) (fst (seqsplit xs)))
      j <- myspawn (parSplitter (depth+1) (snd (seqsplit xs)))
      return (Split (i, j))
  | otherwise = return (End (head xs))

parms :: Ord a => Int -> Merger a -> Merger a -> Par (Merger a)
parms depth ((Split (x, y)), i)          -> spawnSplit x y i depth+1
     
     (i, (Split (x, y)))          -> spawnSplit x y i depth+1
     ((End x), a)                 -> parms (Ordered [x]) a
     (a, (End x))                 -> parms (Ordered [x]) a
     ((Ordered xs), (Ordered ys)) -> return (Ordered (merge (xs, ys))) 

spawnSplit a b i d = do 
                       s <- myspawn (parms (d+1) (Par a) (Par b))
                       f <- get s
                       (parms d f (Par i))

parMerger :: Ord a => Int -> Merger a -> Par [a]
parMerger depth m = case m of
    (End x) -> return [x]
    (Ordered xs) -> return xs
    (Split (x, y)) -> do
                        i <- myspawn (parms depth+1 x y)
                        return (get i)

monadMerger :: (NFData a, Ord a) => [a] -> [a]
monadMerger xs = runPar $ parMergerSort xs 0


divConq :: (NFData prob, NFData sol) => 
                          (prob -> Bool) -> 
                          (prob -> [prob]) -> 
                          ([sol] -> sol) -> 
                          (prob -> sol) ->
                          prob -> Par sol
divConq ndiv split mergec bc xs = go xs
  where 
      go xs 
        | ndiv xs = return (bc xs)
        | otherwise = do 
            sols <- parMapM go (split xs)
            return (mergec sols)

mergeSortCool :: (NFData a, Ord a) => [a] -> [a]
mergeSortCool xs = runPar $ divConq (\xs -> length xs < 2) split mergec id xs
    where split xs = let (a, b) = splitAt (length xs `div` 2) xs
                      in [a, b]
          mergec [] = []
          mergec [as] = as 
          mergec (as:bs:[]) = mergec2 as bs
          mergec2 [] bs = bs
          mergec2 as [] = as
          mergec2 (a:as) (b:bs) | a <= b = a:mergec2 as (b:bs)
                                | a >  b = b:mergec2 (a:as) bs
