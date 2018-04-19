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
    (merge (l1, l2))
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
    (merge (l1', l2'))
