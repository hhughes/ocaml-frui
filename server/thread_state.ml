let seed = 0
let _ = Random.init seed
let latest_tid = ref 0
let next_tid () = let tid = !latest_tid in latest_tid := tid+1; tid

type thread_state = Started | Running | FunEnter | FunExit | Msg | Finished | Stop

class thread =
object (self)
  val tid = next_tid ()
  val mutable in_fun = false
  val mutable state = Started
  val mutable fid = 0
  method enterexit = if in_fun then FunExit else FunEnter
  method stopexit = if in_fun then FunExit else Finished (* dont let us stop when in a function *)
  method next_state p = function
  | Started -> if p < 80 then Running else if p < 90 then self#enterexit else Msg
  | Running -> if p < 40 then Running else if p < 60 then self#enterexit else if p < 90 then Msg else self#stopexit
  | FunEnter -> if p < 50 then Running else if p < 70 then FunExit else Msg
  | FunExit ->  if p < 40 then Running else if p < 50 then self#enterexit else if p < 80 then Msg else self#stopexit
  | Msg -> if p < 60 then Running else if p < 80 then self#enterexit else if p < 90 then Msg else self#stopexit
  | Finished -> Stop
  | Stop -> Stop
  method get_event a = function
  | Started -> (Events.create tid fid Events.TStart) :: a
  | Running -> a
  | FunEnter -> in_fun <- true; (Events.create tid fid Events.FunStart) :: a
  | FunExit -> let i = fid in fid <- i+1; in_fun <- false; (Events.create tid i Events.FunEnd) :: a
  | Msg -> (Events.create tid fid Events.Msg) :: a
  | Finished -> (Events.create tid fid Events.TEnd) :: a
  | Stop -> a
  method next_event a =
    let e = self#get_event a state in
    state <- self#next_state (Random.int 100) state;
    e
  method state = state
end

let threads = ref []

let reset () =
  threads := [];
  latest_tid := 0

let get_events () =
  threads := List.filter (fun t -> t#state <> Stop) !threads;
  if (Random.int 100) < 10 then threads := (new thread) :: !threads;
  let events = List.fold_right (fun t -> t#next_event) !threads [] in
  Json_io.string_of_json (Events.jsonify events)
