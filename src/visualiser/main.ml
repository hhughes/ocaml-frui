open Logger
open JQuery
open Fvar
open Msg
open Printf

let main_elt = (Dom.document#getElementById "main" : Dom.element)
let visualiser_elt = (Dom.document#getElementById "visualiser" : Dom.element)
let pie_elt = (Dom.document#getElementById "pie" : Dom.element)
let cloud_elt = (Dom.document#getElementById "cloud" : Dom.element)
let msg_count_elt = (Dom.document#getElementById "msg_count" : Dom.element)
let timeline_elt = (Dom.document#getElementById "timeline" : Dom.element)

let m0 = new fvar (-1.)
let m1 = new fvar (-1.)

let margin a b = (b -. a) *. 0.05

let t0 = Froc.lift (fun m0 -> m0 -. 5.) m0#b
let t1 = Froc.lift (fun m1 -> m1 +. 5.) m1#b

let pie = Pie.init pie_elt t0 t1
let cloud = Cloud.init cloud_elt t0 t1
let visualiser = Visualiser.init visualiser_elt t0 t1 m0 m1
let msg_count = Msg_count.init msg_count_elt t0 t1
let timeline = Timeline.init timeline_elt m0 m1

let load_pie m = pie#counter#inc (Msg.ty m)

let load_cloud m = 
  let loadword w =
    if not (cloud#counter#mem w) then cloud#new_counter w;
    cloud#counter#inc w
  in
  let name = Msg.name m in
  let desc = Msg.desc m in
  Array.iter loadword (Javascript.Js_string.split name " ");
  Array.iter loadword (Javascript.Js_string.split desc " ")

let load_visualiser m = visualiser#add_to_thread m

let load_msg_count m = msg_count#inc

let load_timeline m = () (* TODO *)

let load_m m =
(*load_pie m *)
    load_cloud m 
(*  load_visualiser m*)
(*  load_msg_count m;
  load_timeline m*)

let load_objects o s =
  match s with
    | "success" -> 
      begin
	let msgs = unmarshall_json o in
	let msgs = js_to_list msgs in
	List.iter load_m msgs
      end
    | _ -> () (*debug s*)

let load_json _ =
  let json_url = 
    let url_input = (Dom.document#getElementById "json_url" : Dom.input) in
    url_input#_get_value
  in
  (*debug json_url;*)
  ignore (jQuery_util#get json_url () load_objects);
  true

let load p f = 
  let json_url = sprintf "http://localhost:8080/%s" p in
  (*debug json_url; *)
  ignore (jQuery_util#get json_url () (fun o s -> load_objects o s; f ()));
  true

let load_next _ = load "next_msg" (fun () -> ())

let load_msg _ = load "msg" (fun () -> ())

let run_id = ref None
let run_count = ref 0

let run () =
  let count = !run_count in
  if count = 0 then Dom.window#clearInterval (match !run_id with Some i -> i)
  else
  run_count := count - 1;
  ignore (load_next ())

let load_start _ = 
  run_count := 200;
  run_id := Some (Dom.window#setInterval run 500.);
  true

let load_test1a _ = load "tests/test1a.json"  (fun () -> ())
let load_test1b _ = load "tests/test1b.json"  (fun () -> ())

let load_test2 n _ = 
  let rec f n () =
    match n with
      | 100 -> ()
      | n -> ignore (load (Printf.sprintf "tests/test2-%d.json" n)  (f (n+1)))
  in
  f n ();
  true

let setup_pie () =
  pie#new_counter "msg";
  pie#new_counter "t_start";
  pie#new_counter "t_finish";
  pie#new_counter "fn_start";
  pie#new_counter "fn_finish"

let onload () =
  let vis_button = Dom.document#getElementById "visualise" in
  vis_button#_set_onclick (load_json);
  let add_button = Dom.document#getElementById "next_msg" in
  add_button#_set_onclick (load_next);
  let start_button = Dom.document#getElementById "start" in
  start_button#_set_onclick (load_start);
  let msg_button = Dom.document#getElementById "msg" in
  msg_button#_set_onclick (load_msg);
  let test1a_button = Dom.document#getElementById "test1a" in
  test1a_button#_set_onclick (load_test1a);
  let test1b_button = Dom.document#getElementById "test1b" in
  test1b_button#_set_onclick (load_test1b);
  let test2_button = Dom.document#getElementById "test2" in
  test2_button#_set_onclick (load_test2 0);
  setup_pie ()
;;

Dom.window#_set_onload onload
