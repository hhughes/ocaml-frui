
type thread_state = Started | Running | FunEnter | FunExit | Msg | Stop

let _ = Random.init (int_of_float (Unix.gettimeofday ()))
let in_fun = ref false

let enterexit () = if !in_fun then FunExit else FunEnter
let stopexit () = if !in_fun then FunExit else Stop (* dont let us stop when in a function *)

let next_state p = function
  | Started -> if p < 80 then Running else if p < 90 then enterexit () else Msg
  | Running -> if p < 40 then Running else if p < 60 then enterexit () else if p < 90 then Msg else stopexit ()
  | FunEnter -> if p < 50 then Running else if p < 70 then FunExit else Msg
  | FunExit ->  if p < 40 then Running else if p > 50 then enterexit () else if p < 80 then Msg else stopexit ()
  | Msg -> if p < 60 then Running else if p < 80 then enterexit () else if p < 90 then Msg else stopexit ()
  | Stop -> Started

let get_event = function
  | Started -> [Events.create_t_start_event ()]
  | Running -> Events.tick (); []
  | FunEnter -> in_fun := true; [Events.create_f_enter_event ()]
  | FunExit -> in_fun:= false; [Events.create_f_exit_event ()]
  | Msg -> [Events.create_msg_event ()]
  | Stop -> [Events.create_t_end_event ()]

let t = ref Started

let get_msg () = Json_io.string_of_json (Events.jsonify [Events.create_msg_event ()])

let rec get_events () =
  let e = get_event !t in
  t := next_state (Random.int 100) !t;
  match e with
    | [] -> get_events ()
    | _ -> Json_io.string_of_json (Events.jsonify e)
