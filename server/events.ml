open Printf
open Json_type
open Build
open Json_io

let count = ref 20

let next_count () =
  let c = !count in
  count := !count + 1; c

let _create_msg () = objekt [
  ("tid", int 0);
  ("ty", array [string "msg"]);
  ("ts", int (next_count ()));
  ("name", string "some name");
  ("desc", string "message1");
  ("misc", objekt [])]

let create_msg () = string_of_json (_create_msg ())
