open Logger
open JQuery
open Msg
open Thread

let main_elt = (Dom.document#getElementById "main" : Dom.element)

let threads = Hashtbl.create 10

let add_to_thread msg =
  let tid = Msg.threadId msg in
  if not (Hashtbl.mem threads tid) then Hashtbl.add threads tid (new thread tid);
  let thread = Hashtbl.find threads tid in
  thread#parse_msg msg

let render_msg (thread_elt : Dom.element) thread msg = 
  let msg_elt = (Dom.document#createElement "div" : Dom.element) in
  ignore (msg_elt#_set_className (Msg.ty msg));
  ignore (msg_elt#_set_title (Msg.desc msg));
  ignore (msg_elt#_get_style#_set_left (string_of_int (10 * ((Msg.timestamp msg) - thread#start))));
  ignore (thread_elt#appendChild msg_elt)

let render_thread id thread =
  let thread_elt = (Dom.document#createElement "div" : Dom.element) in
  ignore (thread_elt#_set_className "thread");
  ignore (thread_elt#_get_style#_set_top (string_of_int (25 * id)));
  ignore (thread_elt#_get_style#_set_left (string_of_int (10 * thread#start)));
  ignore (thread_elt#_get_style#_set_width (string_of_int (10 * (thread#finish - thread#start))));
  (*let thread_text = (Dom.document#createTextNode (string_of_int id) : Dom.text) in
  ignore (thread_elt#appendChild thread_text);*)
  ignore (main_elt#appendChild thread_elt);
  List.iter (render_msg thread_elt thread) thread#msgs

let render_threads () = Hashtbl.iter render_thread threads

let load_objects o s =
  match s with
    | "success" -> 
      begin
	let msgs = unmarshall_json o in
	let add_msg msg = match Msg.ty msg with
	  | "msg"
	  | "t_start"
	  | "t_finish" -> add_to_thread msg
	  | _ -> ()
       	in
	for_each add_msg msgs;
	render_threads ()
      end
    | _ -> debug s

let load_json e =
  let json_url = 
    let url_input = (Dom.document#getElementById "json_url" : Dom.input) in
    url_input#_get_value
  in
  debug json_url;
  ignore (jQuery_util#get json_url () load_objects);
  true

let onload () =
  let vis_button = Dom.document#getElementById "visualise" in
  vis_button#_set_onclick (load_json)
;;

Dom.window#_set_onload onload
