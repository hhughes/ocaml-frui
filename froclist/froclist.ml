open Froc
open Printf

let (>>=) = Froc.(>>=)

(* assert list is always in *time* order *)

let counte,counts = Froc.make_event ()
let count = Froc.hold 0 counte

let inc n = fun c -> return (c + n)

let liste,lists = Froc.make_event ()
let list = Froc.hold [(count >>= inc 2); (count >>= inc 1); (count >>= inc 0)] liste

let rec leq l1 l2 =
  match (l1, l2) with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | x::xs, y::ys -> sample x = sample y & (leq xs ys)

let rec lnew lo ln =
  match (lo, ln) with
    | [], _
    | _, []
    | [], [] -> []
    | x::xs, y::ys ->
      if sample x = sample y then []
      else y :: (lnew lo ys)

let rec lbind l f =
  let s = sample l in
  List.iter (fun x -> ignore (x >>= f)) s;
  l >>= (fun x ->
    if leq s x then return ()
    else lbind (return (lnew s x)) f
  );;

ignore (lbind list (fun i -> printf "item %d\n" i; return ()));;

ignore (send lists [(count >>= inc 4); (count >>= inc 3); (count >>= inc 2); (count >>= inc 1); (count >>= inc 0)]);;

ignore (Froc.send counts ((Froc.sample count) + 1));;
