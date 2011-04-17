type button_type = Up | Down

let create_button (e : Dom.element) (input : Dom.input) ty set_input inc =
  let button = (Dom.document#createElement "button" : Dom.button) in
  let set_value i _ =
    let v = float_of_string input#_get_value in
    input#_set_value (string_of_float (v +. i));
    set_input ();
    true
  in
  ignore (e#appendChild button);
  match ty with
    | Up ->
      button#_set_className "up";
      let button_text = (Dom.document#createTextNode "^" : Dom.text) in
      ignore (button#appendChild button_text);
      button#_set_onclick (set_value inc)
    | Down ->
      button#_set_className "down";
      let button_text = (Dom.document#createTextNode "v" : Dom.text) in
      ignore (button#appendChild button_text);
      button#_set_onclick (set_value (-1. *. inc))

let create e v inc =
  let input = (Dom.document#createElement "input" : Dom.input) in
  let set_input () =
    let i = float_of_string input#_get_value in
    v#set i
  in
  let set_value i =
    input#_set_value (string_of_float i)
  in
  input#_set_value (string_of_float v#get);
  input#_set_onchange set_input;
  input#_set_className "spinner";
  create_button e input Up set_input inc;
  create_button e input Down set_input inc;
  ignore (Froc.lift set_value v#b);
  ignore (e#appendChild input)
