open Logger
open JQuery
open Json

module D = Dom

let load_objects o s =
  match s with
    | "success" -> 
      begin
	let main_elt = (D.document#getElementById "main" : D.element) in
	let msg = unmarshall_json o in
	let text = (D.document#createTextNode (Msg.ty msg) : D.text) in
	ignore (main_elt#appendChild text)
      end
    | _ -> debug s

let load_json e =
  let json_url = 
    let url_input = (D.document#getElementById "json_url" : D.input) in
    url_input#_get_value
  in
  debug json_url;
  ignore (jQuery_util#get json_url () load_objects);
  true

let onload () =
  let vis_button = D.document#getElementById "visualise" in
  vis_button#_set_onclick (load_json)
;;

D.window#_set_onload onload
