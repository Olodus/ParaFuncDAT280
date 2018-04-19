 -- Eval Monad
 
 mergesort2 :: Ord a => [a] -> [a]
 mergesort2 x = runEval (evalMergesort 4 x)
 
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
                   | otherwise  = do  -- before threshold
     --let split = splitInHalf x
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
