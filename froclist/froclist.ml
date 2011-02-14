open Froc
open Printf

let (>>=) = Froc.(>>=)

(* assert list is always in *time* order *)
let liste,lists = Froc.make_event ()
let list = Froc.hold [(return 2); (return 1); (return 0)] liste

let rec leq l1 l2 =
  match (l1,l2) with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | x::xs, y::ys -> sample x = sample y & (leq xs ys)

let rec lbind l f =
  let s = sample l in
  List.iter (fun x -> ignore (x >>= f)) s;
  l >>= (fun x ->
    if leq s x then return ()
    else lbind (return x) f
  );;

ignore (lbind list (fun i -> printf "item %d\n" i; return ()));;

ignore (send lists [(return 3); (return 2); (return 1); (return 0)]);;
