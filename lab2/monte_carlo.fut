import "/futlib/sobol"




-- Exercise 2.1-2.2
let inside (x: f32, y: f32): f32 = 
    let r = ((x-1f32)**2f32) + ((y-1f32)**2f32) 
    let b = r <= 1f32
    in if b then 1f32 else 0f32
 
let estimate_pi [n] (x: [n]f32) (y: []f32): f32 = 
    let c = zip x y
    let r = map inside c
    let s = reduce (+) 0f32 r
    in (s/(r32 n)) * 4f32



-- Exercise 2.3 -
let f (x:f32, y:f32): f32 = 
    2.0f32*x*x*x*x*x*x*y*y - x*x*x*x*x*x*y
    + 3.0f32*x*x*x*y*y*y - x*x*y*y*y +
    x*x*x*y - 3.0f32*x*y*y + x*y -
    5.0f32*y + 2.0f32*x*x*x*x*x*y*y*y*y -
    2.0f32*x*x*x*x*x*y*y*y*y*y + 250f32

let mc_integration [n] (x: [n]f32) (y: []f32): f32 = 
    let c = zip x y
    let r = map f c
    let s = reduce (+) 0f32 r
    in (4f32/(r32 n))*s




-- Exercise 2.4
module S2 = Sobol sobol_dir { let D = 2 }

module R = S2.Reduce { type t = f64
                       let ne = 0f64
                       let op (x:f64) (y:f64) = x f64.+ y
                       let f (v : [2]f64) : f64 =
                         let x = v[0]
                         let y = v[1]
                         in f64.bool(x*x+y*y < 1f64) }

--entry test_pi (n:i32) : f32 =
--  R.run n * 4.0 / r64(n) - 3.14




let main (x: []f32) (y: []f32): f32 = mc_integration  R.run R.run
--let main (): f32 = estimate_pi  x y



--let process_idx (s1: []i32) (s2: []i32): (i32, i32) =
--    let z = zip s1 s2 (iota (length s1))
--    let f = (map (\(x, y, v) -> (((max x y) - (min x y)), v)) z)
--    in reduce (max_with_index) (0,0) f







