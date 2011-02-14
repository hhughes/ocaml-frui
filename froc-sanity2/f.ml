open Froc
open Printf

let (>>=) = Froc.(>>=)

let x = return 1
let y = return 2
let z = return 3

let i0 = x >>= fun x -> y >>= fun y -> return (x+y)
let ans = i0 >>= fun i0 -> z >>= fun z -> return (i0 + z);;

printf "ans = %d\n" (sample ans);;
