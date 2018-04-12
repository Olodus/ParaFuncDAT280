module Solution1 where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par
import Given

-- a) Using par & pseq

-- Add granularity
myParMap :: (a -> b) -> [a] -> [b]
myParMap f [] = []
myParMap f (l:ls) = par x (pseq y (x:y))
    where 
        x = f l
        y = myParMap f ls

parpseqJackknife :: ([a] -> b) -> [a] -> [b]
parpseqJackknife f l = myParMap f $ resamples 500 l


-- b) Eval Monad
evalJackknife :: ([a] -> b) -> [a] -> [b] 
evalJackknife f l = runEval (parMapEval f $ resamples 500 l)

parMapEval :: (a -> b) -> [a] -> Eval [b]
parMapEval _ [] = return []
parMapEval f (x:xs) = do
    y  <- rpar (f x)
    ys <- parMapEval f xs
    return (y:ys)


-- c) Strategies
-- Maybe parFlatMap is the one you want for this...
stratJackknife :: ([a] -> b) -> [a] -> [b]
stratJackknife f xs = map f (resamples 500 xs) `using` jackstrat rseq

evalstrat :: Strategy a -> Strategy [a]
evalstrat strat [] = return []
evalstrat strat (x:xs) = do
    y <- strat x
    ys <- evalstrat strat xs
    return (y:ys)

jackstrat :: Strategy a -> Strategy [a]
jackstrat strat = evalstrat (rparWith strat)



-- d) Par Monad
myspawn :: NFData a => Par a -> Par (IVar a)
myspawn p = do
    i <- new 
    fork (do x <- p; put i x)
    return i

myParMapM :: NFData b => (a -> b) -> [a] -> Par [b]
myParMapM f xs = do
    ibs <- mapM (myspawn . return . f) xs
    mapM get ibs

--monadJackknife :: ([a] -> b) -> [a] -> [b]
--monadJackknife f l = runPar $ do
--    i <- mapM (myspawn . jackshort) l
--    mapM get i

monadJackknife :: NFData b => ([a] -> b) -> [a] -> [b]
monadJackknife f l = runPar $ myParMapM (f) (resamples 500 l)



-- Assignment 2

-- With Strategies
stratMerge :: Ord a => [a] -> [a]
stratMerge l = mergesort l --'using' parPair rpar rpar

-- Borde nog vara något lite mer parPair liknande... 
-- Typ att du skickar rpar till parPair ... och ger rpar rseq...?
--mergeStrat :: Strategy a -> Strategy [a]
--mergeStrat _ [] = []
--mergeStrat strat (l:ls) = do
--    x <- strat l
--    xs <- mergeStrat strat ls
--    return (x:xs)

evalPair :: Strategy a -> Strategy (a,a)
evalPair sa (a,b) = do
    a' <- sa a
    b' <- sa b
    return (a',b')

-- Mergesort without parallism
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort x | l > 1 =  merge $ mapTuple (mergesort) (splitAt (quot l 2) x)
            | otherwise = x
            where l = length x

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (a1,a2) = (f a1, f a2)

merge :: Ord a => ([a],[a]) -> [a]
merge ([], y) = y
merge (x, []) = x
merge ((x:xs), (y:ys)) 
  | x <= y = (x:merge (xs, (y:ys)))
  | x >  y = (y:merge ((x:xs), ys))

-- With Par Monad
--monadMerge :: Ord a => [a] -> [a]
--monadMerge l = runPar $ do parMerge l

mySpawnMerge :: [a] -> Par [a]
mySpawnMerge [] = return []
mySpawnMerge a = get (myspawn (monadMerge a))

monadMerge :: Ord a => [a] -> Par [a]
monadMerge [] = return []
monadMerge x | l > 1 = return mergePar $ mapTuple (mySpawnMerge) (splitAt (quot l 2) x) 
             | otherwise = return x
             where l = length x

mergePar :: Ord a => (Par [a], Par [a]) -> Par [a]
mergePar ((x:xs), (y:ys))
  | x <= y = Par (x:mergePar ((return xs, return (y:ys))))
  | x >  y = Par (y:mergePar ((return (x:xs), return ys)))

--parMerge :: [a] -> [a]
--parMerge a = runPar (monadMerge a)

-- maybe try func composition on myspawnmerge

