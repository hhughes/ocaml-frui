open JQuery
open Json
open Fvar

let main_elt = Dom.document#getElementById "main"

let gdp_data = Hashtbl.create 50
let gdp_vars = Hashtbl.create 240
let gdp_max = ref 0.

let life_data = Hashtbl.create 50
let life_vars = Hashtbl.create 240
let life_max = ref 0.

let min_year = ref 2000
let max_year = ref 2000

let ids = ref []

let load_datum ht max d =
  let year = Datum.date d in
  let country = Item.id (Datum.country d) in
  let value = try float_of_string (Datum.value d) with _ -> nan in
  if not (Hashtbl.mem ht year) then Hashtbl.add ht year [];
  let tl = Hashtbl.find ht year in
  if(year < !min_year) then min_year := year;
  if(year > !max_year) then max_year := year;
  begin
  match classify_float value with
    | FP_normal -> Logger.debug (string_of_float !max); if(value > !max) then max := value else ()
    | _ -> ()
  end;
  Hashtbl.replace ht year ((country,value)::tl)

let load_var ht (id, value) = Hashtbl.add ht id (new fvar value)

let position_div (elt : Dom.element) x_max y_max x y =
  let w = float_of_int (main_elt#_get_offsetWidth) in
  let h = float_of_int (main_elt#_get_offsetHeight) in
  let l = (x *. w) /. x_max in
  let t = y_max -. ((y *. h) /. y_max) in
  Logger.debug (Printf.sprintf "w: %f, h: %f, l:%f, t:%f" w h l t);
  (*Logger.debug (Printf.sprintf "x: %f, max: %f" x x_max);*)
  elt#_get_style#_set_left (string_of_int (int_of_float l));
  elt#_get_style#_set_top (string_of_int (int_of_float t))

let create_div id =
  let elt = Dom.document#createElement "div" in
  elt#_set_className "datum";
  elt#_set_title id;
  let x_axis = Hashtbl.find gdp_vars id in
  let y_axis = Hashtbl.find life_vars id in
  let foo = !gdp_max in
(*  Logger.debug (string_of_float foo);
  Logger.debug (Printf.sprintf "gdp_max: %f, life_max: %f" !gdp_max !life_max);*)
  ignore (Froc.lift2 (position_div elt !gdp_max !life_max) x_axis#b y_axis#b);
  ignore (main_elt#appendChild elt)

(* HACK *)
let to_load = ref 1
let loaded () =
  Logger.debug "load";
  if !to_load > 0 then to_load := !to_load - 1
  else List.iter create_div !ids

let load_objects ht_d ht_v max o s =
  match s with
    | "success" ->
      begin
	let data = js_to_list (unmarshall_json o) in (* expensive!? *)
	List.iter (load_datum ht_d max) data;
	List.iter (load_var ht_v) (Hashtbl.find ht_d !max_year);
	List.iter (fun (id,_) -> ids := id :: !ids) (Hashtbl.find ht_d !max_year);
	loaded ()
      end
    | _ -> Logger.debug s

let load_json _ =
  let urls = [("http://localhost:8080/gdp", gdp_data, gdp_vars, gdp_max);
	      ("http://localhost:8080/life", life_data, life_vars, life_max)] in
  List.iter (fun (u, ht_d, ht_v, max) -> ignore (jQuery_util#get u () (load_objects ht_d ht_v max))) urls;
  true

let onload () =
  let load_button = Dom.document#getElementById "load" in
  load_button#_set_onclick (load_json)

;;

Dom.window#_set_onload onload

