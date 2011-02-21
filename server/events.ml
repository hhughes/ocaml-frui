open Printf
open Json_type
open Build
open Json_io

let _ = Random.init (int_of_float (Unix.gettimeofday ()))

type event_type = Msg | FunStart | FunEnd | TStart | TEnd

type event = {
  tid: int;
  ty: event_type;
  ts: float;
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
    ("ts", float event.ts);
    ("name", string event.name);
    ("desc", string event.desc);
    ("misc", objekt [])] 

let jsonify events = array (List.map _jsonify events)

let get_thread () = Random.int 10

let fn = ref 0
let time = Unix.gettimeofday
let inc c = let v = !c in c := v+1; v

let create tid ty =
  let ts = time () in
  match ty with
    | TStart
    | TEnd -> create_event tid ty ts "t" "t"
    | Msg -> create_event tid ty ts "m" "m"
    | FunStart -> create_event tid ty ts (sprintf "f%d" !fn) "f"
    | FunEnd -> create_event tid ty ts (sprintf "f%d" (inc fn)) "f"
