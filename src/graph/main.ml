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

let time = new fvar 2000.

let ids = ref []

let load_datum ht max d =
  let year = int_of_string (Datum.date d) in
  let country = Item.id (Datum.country d) in
  let value = try float_of_string (Datum.value d) with _ -> nan in
  if not (Hashtbl.mem ht year) then Hashtbl.add ht year [];
  let tl = Hashtbl.find ht year in
  if(year < !min_year) then min_year := year;
  if(year > !max_year) then max_year := year;
  begin
  match classify_float value with
    | FP_normal -> if(value > !max) then max := value else ()
    | _ -> ()
  end;
  Hashtbl.replace ht year ((country,value)::tl)

let load_var ht (id, value) = Hashtbl.replace ht id (new fvar value)

let position_div (elt : Dom.element) x_max y_max x y =
  let w = float_of_int (main_elt#_get_offsetWidth) in
  let h = float_of_int (main_elt#_get_offsetHeight) in
  let l = (x *. w) /. x_max in
  let t = h -. ((y *. h) /. y_max) in
  elt#_get_style#_set_left (string_of_int (int_of_float l));
  elt#_get_style#_set_top (string_of_int (int_of_float t))

let create_div id =
  let elt = Dom.document#createElement "div" in
  elt#_set_className "datum";
  elt#_set_title id;
  let x_axis = Hashtbl.find gdp_vars id in
  let y_axis = Hashtbl.find life_vars id in
(*  let foo = !gdp_max in
  Logger.debug (string_of_float foo);
  Logger.debug (Printf.sprintf "gdp_max: %f, life_max: %f" !gdp_max !life_max);*)
  ignore (Froc.lift2 (position_div elt !gdp_max !life_max) x_axis#b y_axis#b);
  ignore (main_elt#appendChild elt)

let update_axis ht_d ht_v y =
  let update (id,v) =
    let var = Hashtbl.find ht_v id in
    var#set v
  in
  List.iter update (Hashtbl.find ht_d (int_of_float y))
(* HACK *)
let to_load = ref 99
let loaded () =
  if !to_load > 0 then to_load := !to_load - 1
  else List.iter create_div !ids

let load_objects ht_d ht_v max o s =
  match s with
    | "success" ->
      begin
	let data = js_to_list (unmarshall_json o) in (* expensive!? *)
	let data = js_to_list (List.hd data) in
	List.iter (load_datum ht_d max) data;
	List.iter (load_var ht_v) (Hashtbl.find ht_d 2009);
	ids := [];
	List.iter (fun (id,_) -> ids := id :: !ids) (Hashtbl.find ht_d 2009);
	ignore(Froc.lift (update_axis ht_d ht_v) time#b);
	loaded ()
      end
    | _ -> Logger.debug s

let rec get_n u d v m = function
  | 0 -> ()
  | n -> 
    let url = Printf.sprintf u n in
    Logger.debug url;
    ignore (jQuery_util#get url () (load_objects d v m));
    get_n u d v m (n-1)

let load_json _ =
  let n_input = (Dom.document#getElementById "n" : Dom.input) in
  let n = int_of_string (n_input#_get_value) + 1 in
  to_load := (2 * n) - 1;
  get_n "http://localhost:8080/gdp/gdp-%d.json" gdp_data gdp_vars gdp_max n;
  get_n "http://localhost:8080/life/life-%d.json" life_data life_vars life_max n;
  true

let onload () =
  let load_button = Dom.document#getElementById "load" in
  load_button#_set_onclick (load_json);
  let spinner_div = Dom.document#getElementById "spinner" in
  Spinner.create spinner_div time 1.
;;

Dom.window#_set_onload onload

