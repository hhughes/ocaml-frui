type button_type = Up | Down

let create_button (e : Dom.element) (input : Dom.input) ty set_input =
  let button = (Dom.document#createElement "button" : Dom.button) in
  let set_value i _ =
    let v = int_of_string input#_get_value in
    input#_set_value (string_of_int (v + i));
    set_input ();
    true
  in
  ignore (e#appendChild button);
  match ty with
    | Up ->
      button#_set_className "up";
      let button_text = (Dom.document#createTextNode "^" : Dom.text) in
      ignore (button#appendChild button_text);
      button#_set_onclick (set_value 1)
    | Down ->
      button#_set_className "down";
      let button_text = (Dom.document#createTextNode "v" : Dom.text) in
      ignore (button#appendChild button_text);
      button#_set_onclick (set_value (-1))

let create e v =
  let input = (Dom.document#createElement "input" : Dom.input) in
  let set_input () =
    let i = int_of_string input#_get_value in
    v#set (float_of_int i)
  in
  let set_value i =
    let x = int_of_float i in
    input#_set_value (string_of_int x)
  in
  input#_set_value (string_of_int (int_of_float v#get));
  input#_set_onchange set_input;
  input#_set_className "spinner";
  create_button e input Up set_input;
  create_button e input Down set_input;
  ignore (Froc.lift set_value v#b);
  ignore (e#appendChild input)
