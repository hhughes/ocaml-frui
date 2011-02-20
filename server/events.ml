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
let fns = Hashtbl.create 10

let time = Unix.gettimeofday

let create ty =
  let tid = get_thread () in
  let ts = time () in
  match ty with
    | TStart
    | TEnd -> create_event tid ty ts "t" "t"
    | Msg -> create_event tid ty ts "m" "m"
    | FunStart ->
      if not (Hashtbl.mem fns tid) then Hashtbl.add fns tid (-1,[]);
      let c,s = Hashtbl.find fns tid in
      Hashtbl.replace fns tid (c+1, c+1 :: []);
      create_event tid ty ts (sprintf "f%d" c) "f"
    | FunEnd -> 
      if Hashtbl.mem fns tid then
	begin
	  let c,s = Hashtbl.find fns tid in
	  if List.length s > 0 then
	    begin
	      Hashtbl.replace fns tid (c, List.tl s);
	      create_event tid ty ts (sprintf "f%d" (List.hd s)) "f"
	    end
	  else create_event tid Msg ts "no_open_fns" "no_open_fns"
	end
      else create_event tid Msg ts "no_thread" "no_thread"

(*let create_msg () = string_of_json (jsonify (create_events ()))*)
