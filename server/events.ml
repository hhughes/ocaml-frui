open Printf
open Json_type
open Build
open Json_io

type event_type = Msg | FunStart | FunEnd | TStart | TEnd

type event = {
  tid: int;
  ty: event_type;
  ts: int;
  name: string;
  desc: string;
}

let create_event tid ty ts name desc = {
  tid = tid;
  ty = ty;
  ts = ts;
  name = name;
  desc = desc;
}

let string_of_event_type = function
  | Msg -> "msg"
  | FunStart -> "fn_start"
  | FunEnd -> "fn_finish"
  | TStart -> "t_start"
  | TEnd -> "t_finish"

let _jsonify event = objekt [
    ("tid", int event.tid);
    ("ty", string (string_of_event_type event.ty));
    ("ts", int event.ts);
    ("name", string event.name);
    ("desc", string event.desc);
    ("misc", objekt [])] 

let jsonify events = array (List.map _jsonify events)

let count = ref 0
let thread = ref 0
let fn = ref 0

let next count =
  let c = !count in
  count := !count + 1; c

let tick () = next count

let create_t_start_event () = create_event !thread TStart (next count) "t" "t"

let create_t_end_event () = create_event (next thread) TEnd (next count) "t" "t"

let create_msg_event () = create_event !thread Msg (next count) "m" "m"

let create_f_enter_event () = create_event !thread FunStart (next count) (sprintf "f%d" !fn) "f"

let create_f_exit_event () = create_event !thread FunEnd (next count) (sprintf "f%d" (next fn)) "f"

(*let create_msg () = string_of_json (jsonify (create_events ()))*)
