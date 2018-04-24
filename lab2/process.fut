let s1 = [23,45,-23,44,23,54,23,12,34,54,7,2, 4,67]
let s2 = [-2, 3, 4,57,34, 2, 5,56,56, 3,3,5,77,89]
let t1 = [1, 2, 3]
let t2 = [-1, -2, -3]

let max (x: i32) (y: i32): i32 =
    if x > y then x
    else y

let min (x: i32) (y: i32): i32 =
    if x < y then x
    else y

let process (s1: []i32) (s2: []i32): i32 =
    let z = zip s1 s2
    in reduce (max) 0 (map (\(x, y) -> (max x y) - (min x y) ) z)
    

let main (x: []i32) (y: []i32): i32 = process  x y

