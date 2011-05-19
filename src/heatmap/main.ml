open Fvar
open Printf
open Javascript
open Elec
open JQuery

external new_Object : string -> elec js_array = "@eval"
let unmarshall_json json = new_Object json
let rec js_to_list msgs =
  if msgs#_get_length > 0 then
    msgs#pop :: (js_to_list msgs)
  else []

let main_elt = Dom.document#getElementById "main"

let elec_data = Hashtbl.create 50
let elec_vars = Hashtbl.create 10
let elec_max = ref 0.

let max_date = ref 0.
let min_date = ref 0.

let time = new fvar 0.
let inc = 3600000.

let ids = ref []

let pos = function
  | "GN17" -> ("470", "90")
  | "FN30" -> ("1120", "70")
  | "SN30" -> ("1730", "70")
  | _ -> ("0", "0")

let load_datum ht max d =
  let room = Elec.room d in
  let data = js_to_list (Elec.data d) in
  let load_d l =
    let l = js_to_list l in
    let date = float_of_string (List.nth l 1) in
    let value = float_of_string (List.nth l 0) in
    let tl = try Hashtbl.find ht date with _ -> [] in
    if((date < !min_date) or (!min_date = 0.)) then min_date := date;
    if(date > !max_date) then max_date := date;
    if(value > !max) then max := value;
    Hashtbl.replace ht date ((room,value)::tl)
  in
  List.iter load_d data

let load_var ht (id, value) = Hashtbl.add ht id (new fvar value)

let colour_div (elt : Dom.element) id time =
  Logger.debug "setting colour";
  let max = !elec_max in
  let vals = Hashtbl.find elec_data time in 
  let value = List.assoc id vals in (* really slow... *)
  Logger.debug (sprintf "t: %f, v: %f" time value);
  let p = int_of_float ((value *. 255.) /. max) in
  elt#_get_style#_set_backgroundColor (sprintf "rgb(%d,%d,0)" p (255 - p))

let create_div id =
  let elt = Dom.document#createElement "div" in
  let left, top = pos id in
  elt#_set_className "room";
  elt#_set_title id;
  elt#_get_style#_set_left left;
  elt#_get_style#_set_top top;
  ignore (Froc.lift (colour_div elt id) time#b);
  Logger.debug "appending child";
  ignore (main_elt#appendChild elt)

let run () = time#set (time#get +. inc)

(* HACK *)
let to_load = ref 2
let loaded () =
  Logger.debug "load";
  time#set !min_date;
  if !to_load > 0 then to_load := !to_load - 1
  else List.iter create_div !ids
  

let load_objects ht_d ht_v max o s =
  match s with
    | "success" ->
      begin
	let data = js_to_list (unmarshall_json o) in (* expensive!? *)
	Logger.debug "unmarshall";
	List.iter (load_datum ht_d max) data;
	Logger.debug "data loaded";
	List.iter (load_var ht_v) (Hashtbl.find ht_d !max_date);
	Logger.debug "vars loaded";
	ids := [];
	List.iter (fun (id,_) -> Logger.debug id; ids := id :: !ids) (Hashtbl.find ht_d !max_date);
	Logger.debug "ids loaded";
	loaded ()
      end
    | _ -> () (*Logger.debug s*)

let load_json _ =
  let n_input = (Dom.document#getElementById "n" : Dom.input) in
  let n = int_of_string (n_input#_get_value) in
  to_load := n-1;
  let urls = match n with
    | 1 -> [("http://localhost:8080/elec", elec_data, elec_vars, elec_max);]
    | 2 -> [("http://localhost:8080/elec", elec_data, elec_vars, elec_max);
	      ("http://localhost:8080/elec2", elec_data, elec_vars, elec_max)]
    | _ -> [("http://localhost:8080/elec", elec_data, elec_vars, elec_max);
	      ("http://localhost:8080/elec2", elec_data, elec_vars, elec_max);
	      ("http://localhost:8080/elec3", elec_data, elec_vars, elec_max)] in
  List.iter (fun (u, ht_d, ht_v, max) -> ignore (jQuery_util#get u () (load_objects ht_d ht_v max))) urls;
  true

let onload () =
  let load_button = Dom.document#getElementById "load" in
  load_button#_set_onclick (load_json);
  let spinner_div = Dom.document#getElementById "spinner" in
  Spinner.create spinner_div time inc;
  let play_div = Dom.document#getElementById "play" in
  play_div#_set_onclick (fun _ -> ignore (Dom.window#setInterval run 0.); true)

;;

Dom.window#_set_onload onload

