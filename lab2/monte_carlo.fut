--x = []
--y = []


let inside (x: f32, y: f32): f32 = 
    let r = ((x-1f32)**2f32) + ((y-1f32)**2f32) 
    let b = r <= 1f32
    in if b then 1f32 else 0f32
 
let estimate_pi [n] (x: [n]f32) (y: []f32): f32 = 
    let c = zip x y
    let r = map inside c
    let s = reduce (+) 0f32 r
    in (s/(r32 n)) * 4f32

let main (x: []f32) (y: []f32): f32 = estimate_pi  x y
--let main (): f32 = estimate_pi  x y



--let process_idx (s1: []i32) (s2: []i32): (i32, i32) =
--    let z = zip s1 s2 (iota (length s1))
--    let f = (map (\(x, y, v) -> (((max x y) - (min x y)), v)) z)
--    in reduce (max_with_index) (0,0) f







