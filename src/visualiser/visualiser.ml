open Logger
open JQuery
open Msg
open Thread
open Printf
open Fvar

let (>>=) = Froc.(>>=)

let main_elt = (Dom.document#getElementById "main" : Dom.element)

let threads = Hashtbl.create 10
let mc = new fvar 0

let m0 = new fvar (-1)
let m1 = new fvar (-1)

let margin a b = ((b - a) * 5) / 100

let t0 = Froc.lift2 (fun m0 m1 -> debug "t0 updated"; m0 - (margin m0 m1)) m0#b m1#b
let t1 = Froc.lift2 (fun m0 m1 -> debug "t1 updated"; m1 + (margin m0 m1)) m0#b m1#b

let w () = main_elt#_get_offsetWidth - 4 (* would be nice if this were froc-ed *)

let add_msg_count () =
  let count_elt = (Dom.document#createElement "div" : Dom.element) in
  let count_txt = (Dom.document#createTextNode "" : Dom.text ) in
  ignore (Froc.lift (fun c -> ignore (count_txt#_set_data (string_of_int c))) mc#b);
  ignore (count_elt#_set_className "count");
  ignore (count_elt#appendChild count_txt);
  ignore (main_elt#appendChild count_elt)

let set_msg_loc msg msg_elt s t0 t1 =
  let d = t1 - t0 in
  let w = w () in
  let ts = Msg.timestamp msg in
  let l = ((ts - t0) * w) / d in
  let x = ((s - t0) * w) / d in
  debug (sprintf "updating msg t=%d t0=%d t1=%d" ts t0 t1);
  ignore (msg_elt#_get_style#_set_left (string_of_int (l - x)))

let set_fun_loc f msg_elt fs fe ts t0 t1 =
  let fe = if fe < 0 then m1#get else fe in 
  let d = t1 - t0 in
  let w = w () in
  let l = ((fs - t0) * w) / d in
  let r = ((fe - t0) * w) / d in
  let x = ((ts - t0) * w) / d in
  ignore (msg_elt#_get_style#_set_left (string_of_int (l - x)));
  ignore (msg_elt#_get_style#_set_width (string_of_int (r - l)))

let set_thread_loc thread_elt ts tf t0 t1 =
  debug (sprintf "updating thread t0=%d t1=%d" t0 t1);
  let tf = if tf < 0 then m1#get else tf in
  let d = t1 - t0 in
  let w = w () in
  let l = ((ts - t0) * w) / d in
  let r = ((tf - t0) * w) / d in
  ignore (thread_elt#_get_style#_set_left (string_of_int l));
  ignore (thread_elt#_get_style#_set_width (string_of_int (r - l)))

let render_msg (thread_elt : Dom.element) thread msg = 
  debug ("creating new event div");
  let msg_elt = (Dom.document#createElement "div" : Dom.element) in
  (match msg with
    | E_msg m ->
      begin
	ignore (msg_elt#_set_className "msg");
	ignore (msg_elt#_set_title (Msg.desc m));
	ignore (Froc.lift3 (set_msg_loc m msg_elt) thread#start#b t0 t1);
      end
    | E_fn f ->
      begin
	ignore (msg_elt#_set_className "fn");
	ignore (msg_elt#_set_title f#name);
	ignore (Froc.lift5 (set_fun_loc f msg_elt) f#start#b f#finish#b thread#start#b t0 t1);
      end
    | Dummy -> ());
  ignore (thread_elt#appendChild msg_elt)

let render_thread id thread =
  let thread_elt = (Dom.document#createElement "div" : Dom.element) in
  ignore (thread_elt#_set_className "thread");
  ignore (thread_elt#_get_style#_set_top (string_of_int (25 * id)));
  ignore (Froc.lift4 (set_thread_loc thread_elt) thread#start#b thread#finish#b t0 t1);
  (*let thread_text = (Dom.document#createTextNode (string_of_int id) : Dom.text) in
  ignore (thread_elt#appendChild thread_text);*)
  ignore (main_elt#appendChild thread_elt);
  thread_elt

let add_to_thread msg =
  debug (sprintf "event of type %s" (Msg.ty msg));
  let tid = Msg.threadId msg in
  (if not (Hashtbl.mem threads tid) then 
    begin
      let thread = new thread tid in
      Hashtbl.add threads tid (thread);
      let thread_elt = render_thread tid thread in
      ignore (Froc.lift (render_msg thread_elt thread) thread#latest_msg#b)
    end);
  let thread = Hashtbl.find threads tid in
  thread#parse_msg msg;
  let ts = Msg.timestamp msg in
  if ts >= 0 then
    begin
      if ts < m0#get or m0#get = (-1) then m0#set ts;
      if ts > m1#get or m1#get = (-1) then m1#set ts;
    end;
  mc#set (mc#get + 1)

let load_objects o s =
  match s with
    | "success" -> 
      begin
	let msgs = unmarshall_json o in
	let msgs = js_to_list msgs in
	List.iter add_to_thread msgs
      end
    | _ -> debug s

let load_json _ =
  let json_url = 
    let url_input = (Dom.document#getElementById "json_url" : Dom.input) in
    url_input#_get_value
  in
  debug json_url;
  ignore (jQuery_util#get json_url () load_objects);
  true

let load p = 
  let json_url = sprintf "http://localhost:8080/%s" p in
  debug json_url; 
  ignore (jQuery_util#get json_url () load_objects);
  true

let load_next _ = load "next_msg"

let load_msg _ = load "msg"

let run () = ignore (load_next ())

let load_start _ = 
  ignore (Dom.window#setInterval run 5.);
  true

let onload () =
  let vis_button = Dom.document#getElementById "visualise" in
  vis_button#_set_onclick (load_json);
  let add_button = Dom.document#getElementById "next_msg" in
  add_button#_set_onclick (load_next);
  let start_button = Dom.document#getElementById "start" in
  start_button#_set_onclick (load_start);
  let msg_button = Dom.document#getElementById "msg" in
  msg_button#_set_onclick (load_msg);
  add_msg_count ()
;;

Dom.window#_set_onload onload
