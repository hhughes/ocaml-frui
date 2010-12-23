module D = Dom
module F = Froc
module Fd = Froc_dom

let onload () =
  let main = (D.document#getElementById "main" : D.element) in
  let hello = (D.document#createElement "div" : D.element) in
  let text = (D.document#createTextNode "hw_text" : D.text) in
  text#_set_data "Hello World!";
  ignore (hello#appendChild text);
  hello#_get_style#_set_position "absolute";
  let mouse = Fd.mouse_b () in
  Fd.appendChild main
    (F.blift mouse (fun (x,y) ->
      hello#_get_style#_set_left (string_of_int x);
      hello#_get_style#_set_top (string_of_int y);
      hello))

;;

D.window#_set_onload onload
