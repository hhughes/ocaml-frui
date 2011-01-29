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

let count = ref 20

let next_count () =
  let c = !count in
  count := !count + 1; c


let create_events () = [
  create_event 0 Msg (next_count ()) "some name" "message";
  create_event 2 TEnd (next_count ()) "t2" "t2";
  create_event 2 Msg (next_count ()) "some name" "message";
  create_event 2 TStart (next_count ()) "t2" "t2";
]

let create_msg () = string_of_json (jsonify (create_events ()))
