open Printf
open Json_type
open Build
open Json_io

type msg_type = Msg | FunStart | FunEnd | TStart | TEnd

type msg = {
  tid: int;
  ty: msg_type;
  ts: int;
  name: string;
  desc: string;
}

let create_msg tid ty ts name desc = {
  tid = tid;
  ty = ty;
  ts = ts;
  name = name;
  desc = desc;
}

let string_of_msg_type = function
  | Msg -> "msg"
  | FunStart -> "fn_start"
  | FunEnd -> "fn_finish"
  | TStart -> "t_start"
  | TEnd -> "t_finish"

let _jsonify msg = objekt [
    ("tid", int msg.tid);
    ("ty", string (string_of_msg_type msg.ty));
    ("ts", int msg.ts);
    ("name", string msg.name);
    ("desc", string msg.desc);
    ("misc", objekt [])] 

let jsonify msgs = array (List.map _jsonify msgs)

let count = ref 20

let next_count () =
  let c = !count in
  count := !count + 1; c

(*let _create_fn_start () =*)

let _create_msg () = jsonify [create_msg 0 Msg (next_count ()) "some name" "message"]

let create_msg () = string_of_json (_create_msg ())
