let s1 = [23,45,-23,44,23,54,23,12,34,54,7,2, 4,67]
let s2 = [-2, 3, 4,57,34, 2, 5,56,56, 3,3,5,77,89]

let process (s1: []i32) (s2: []i32): i32 =
    let z = zip s1 s2
    --let f = (\x y -> if x < y then y-x else x-y)
    let m = map (\x -> if x < y then y-x else x-y) z
    in reduce + m
    --reduce + (map (\(x y) -> if x < y then y-x else x-y) (zip s1 s2))
    


let main (): i32 = process s1 s2

