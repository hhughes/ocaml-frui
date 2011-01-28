open Logger
open JQuery
open Msg
open Thread
open Printf

let (>>=) = Froc.(>>=)

let main_elt = (Dom.document#getElementById "main" : Dom.element)

let threads = Hashtbl.create 10
let mce,mcs = Froc.make_event ()
let mcb = Froc.hold 0 mce

let add_to_thread msg =
  let tid = Msg.threadId msg in
  if not (Hashtbl.mem threads tid) then Hashtbl.add threads tid (new thread tid);
  let thread = Hashtbl.find threads tid in
  thread#parse_msg msg;
  Froc.send mcs ((Froc.sample mcb) + 1)

let add_msg_count () =
  let count_elt = (Dom.document#createElement "div" : Dom.element) in
  let count_txt = (Dom.document#createTextNode "" : Dom.text ) in
  ignore (Froc.notify_b mcb (fun c -> ignore (count_txt#_set_data (string_of_int c))));
  ignore (count_elt#_set_className "count");
  ignore (count_elt#appendChild count_txt);
  ignore (main_elt#appendChild count_elt)

let set_msg_loc msg msg_elt s = ignore (msg_elt#_get_style#_set_left (string_of_int (10 * ((Msg.timestamp msg) - s))))

let set_thread_loc thread_elt s f =
    debug (sprintf "thread updated: %d - %d" s f);
    ignore (thread_elt#_get_style#_set_left (string_of_int (10 * s)));
    ignore (thread_elt#_get_style#_set_width (string_of_int (10 * (f - s))))

let render_msg (thread_elt : Dom.element) thread msg = 
  let msg_elt = (Dom.document#createElement "div" : Dom.element) in
  (match msg with
    | E_msg m ->
      begin
	ignore (msg_elt#_set_className "msg");
	ignore (msg_elt#_set_title (Msg.desc m));
	ignore (Froc.lift (set_msg_loc m msg_elt) thread#start#b);
	set_msg_loc m msg_elt thread#start#sample
      end
    |E_fn f ->
      begin
	ignore (msg_elt#_set_className "fn");
	ignore (msg_elt#_set_title f#name);
	ignore (msg_elt#_get_style#_set_left (string_of_int (10 * (f#start - thread#start#sample))));
	ignore (msg_elt#_get_style#_set_width (string_of_int (10 * (f#finish - (f#start + thread#start#sample)))))
      end);
  ignore (thread_elt#appendChild msg_elt)

let render_thread id thread =
  let thread_elt = (Dom.document#createElement "div" : Dom.element) in
  ignore (thread_elt#_set_className "thread");
  ignore (thread_elt#_get_style#_set_top (string_of_int (25 * id)));
  ignore (Froc.lift2 (set_thread_loc thread_elt) thread#start#b thread#finish#b);
  set_thread_loc thread_elt thread#start#sample thread#finish#sample;
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
	for_each add_to_thread msgs;
	render_threads ()
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

let load_next _ =
  let json_url = "http://localhost:8080/next_msg" in
  debug json_url; 
  ignore (jQuery_util#get json_url () load_objects);
  true

let onload () =
  let vis_button = Dom.document#getElementById "visualise" in
  vis_button#_set_onclick (load_json);
  let add_button = Dom.document#getElementById "next_msg" in
  add_button#_set_onclick (load_next);
  add_msg_count ()
;;

Dom.window#_set_onload onload
