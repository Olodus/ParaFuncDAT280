let s1 = [23,45,-23,44,23,54,23,12,34,54,7,2, 4,67]
let s2 = [-2, 3, 4,57,34, 2, 5,56,56, 3,3,5,77,89]
let t1 = [20,4,5]
let t2 = [0,0,3]

let max (x: i32) (y: i32): i32 =
    if x > y then x
    else y

let min (x: i32) (y: i32): i32 =
    if x < y then x
    else y

let process (s1: []i32) (s2: []i32): i32 =
    reduce_comm (max) 0 
        (map (\(x, y) -> (max x y) - (min x y) ) 
            (zip s1 s2))

--let max_with_index (x: i32, z: i32) (y: i32, v: i32): (i32, i32) =
--    if x > y then (x, z)
--    else (y, v)

let process_idx [n] (s1: [n]i32) (s2: []i32): (i32, i32) =  
    --let z = zip s1 s2 (iota (length s1))
    --let f = (map (\(x, y, v) -> (((max x y) - (min x y)), v)) z)
    --in reduce (max_with_index) (0,0) f
    reduce_comm (\(x,i1) (y,i2) -> if x > y then (x,i1) else (y,i2)) (0,0)
        (map (\(x, y, v) -> (((max x y) - (min x y)), v))
            (zip s1 s2 (iota n)))

-- run exercise 1.2
-- let main (x: []i32) (y: []i32): i32 = process  x y


-- run exercise 1.3
-- let main (x: []i32) (y: []i32): (i32, i32) = process_idx x y
-- let main (): (i32, i32) = process_idx s1 s2
-- results 
-- 73i32
-- 12i32


-- Exercise 1.5

let neuelem 't (op: t -> t -> t) (v1: t, f1: bool) (v2: t, f2: bool): (t, bool) =
    let f = f1 || f2
    in if f2 then (v2, f) else (op v1 v2, f)

let segscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): [n]t =
    --let r = (\((aggr, b1), (x, b2)) -> if b2 then (x, b1 || b2) else ((o aggr x), b1 || b2))
    --let f = (\((aggr, f1), (x, f2)) -> 
    --    let f = f1 || f2
    --    let 
    --let t = (\((aggr, b1), (x, b2)) -> if 
    let (r, _) = unzip (scan (neuelem op) (ne, true) arr)
    in r

-- test segscan
let main : []i32 = segscan (+) (0) [(1,false),(2,false),(3,true),(4,false)]
-- result [1, 3, 3, 7]

--let segreduce [n] ’t (op: t -> t -> t) (ne: t)
--                     (arr: [n](t, bool)): []t =
--    let s = segscan op ne arr
--    scatter unzip (scan (neuelem op) (ne, false) arr)
--    length (filter (\(x, b) -> b) (scan (neuelem op) (ne, false) arr))

