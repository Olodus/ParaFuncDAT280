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
    let z = zip s1 s2
    in reduce (max) 0 (map (\(x, y) -> (max x y) - (min x y) ) z)



    
let max_with_index (x: i32, z: i32) (y: i32, v: i32): (i32, i32) =
    if x > y then (x, z)
    else (y, v)

let process_idx (s1: []i32) (s2: []i32): (i32, i32) =  
    let z = zip s1 s2 (iota (length s1))
    let f = (map (\(x, y, v) -> (((max x y) - (min x y)), v)) z)
    in reduce (max_with_index) (0,0) f

-- run exercise 1.2
-- let main (x: []i32) (y: []i32): i32 = process  x y


-- run exercise 1.3
-- let main (x: []i32) (y: []i32): (i32, i32) = process_idx x y
-- let main (): (i32, i32) = process_idx s1 s2
-- results 
-- 73i32
-- 12i32


-- Exercise 1.5


let segscan [n] i32 (op: i32 -> i32 -> i32) (ne: i32) (arr: [n](i32, bool)): [n]i32 =
    let r = (\ (o, (aggr, b1), (x, b2)) -> if b2 then (x, b2) else ((o aggr x), b2) )
    in scan r (op, (0, false)) (zip arr op)
    



--let segreduce [n] ’t (op: t -> t -> t) (ne: t)
--                     (arr: [n](t, bool)): []t =
