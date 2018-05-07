
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

let process_idx [n] (s1: [n]i32) (s2: []i32): (i32, i32) =  
    reduce_comm (\(x,i1) (y,i2) -> if x > y then (x,i1) else (y,i2)) (0,0)
        (map (\(x, y, v) -> (((max x y) - (min x y)), v))
            (zip s1 s2 (iota n)))

-- Tests
-- Testing arrays
--let s1 = [23,45,-23,44,23,54,23,12,34,54,7,2, 4,67]
--let s2 = [-2, 3, 4,57,34, 2, 5,56,56, 3,3,5,77,89]

-- test exercise 1.1
--let main : i32 = process  s1 s2

-- test exercise 1.2
--let main (x: []i32) (y: []i32): i32 = process  x y

-- test exercise 1.3
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
    let (r, _) = unzip (scan (neuelem op) (ne, false) arr)
    in r

let segment 't (op: t -> t -> t) (v1: t, _: bool) (v2: t, f2: bool): (t, bool) =
    if f2 then (v2, f2) else (op v1 v2, f2)


let segreduce [n] 't (op: t -> t -> t) (ne: t)
                     (arr: [n](t, bool)): []t =
    let (r, s) = unzip (scan (segment op) (ne, false) arr)
    let (r2, _) = unzip (filter (\ (_,b) -> b == true) (zip r ((tail s) ++ [false])))
    in r2 ++ [(last r)]

-- We created another since we didn't get it to run as fast as we wanted. 
-- We kept it in even though it didn't live up to our expectations.
let segreduce2 [n] 't (op: t -> t -> t) (ne: t)
                     (arr: [n](t, bool)): []t =
    let (r, s) = unzip (scan (segment op) (ne, false) arr)
    let (r2, _) = unzip (filter (\ (_,b) -> b == true) (zip r (rotate@0 (1) s)))
    in r2

-- Tests
-- test segscan
--let main (arr1: []i32) (arr2: []bool): []i32 = segscan (+) (0) (zip arr1 arr2)
--let main : []i32 = segscan (+) (0) [(1,false),(2,false),(3,true),(4,false),(5,false)]
-- result [1, 3, 3, 7]

-- test scan
--let main (x: []i32): []i32 = scan (+) (0) x

-- test segreduce
--let main : []i32 = segreduce (+) (0) [(1,true),(2,false),(3,true),(4,false),(5,false),(3,true),(4,false),(5,false)]
-- result [3, 12, 12]
--let main (arr1: []i32) (arr2: []bool): []i32 = segreduce (+) (0) (zip arr1 arr2)



