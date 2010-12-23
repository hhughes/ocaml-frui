open Ocamljs.Inline

module D = Dom
module F = Froc
module Fd = Froc_dom

class logger =
object
  method log (msg : string) =  ignore(<:exp< console.log($msg$) >>)
end

class dialog (elt : D.element) =
object (self)
  val dialog = (D.document#createElement "div")
  method add_title =
    let title = (D.document#createElement "div") in
    let style = title#_get_style in
    ignore (style#_set_border "1px solid black");
    let close = (D.document#createElement "div") in
    let title_text = (D.document#createTextNode "" : D.text) in
    title_text#_set_data "title";
    ignore (title#appendChild title_text);
    let close_text = (D.document#createTextNode "" : D.text) in
    close_text#_set_data "x";
    let close_style = close#_get_style in
    ignore (close_style#_set_display "inline-block");
    ignore (close_style#_set_position "absolute");
    ignore (close_style#_set_right "5px");
    ignore (close_style#_set_cursor "hand");
    close#_set_onclick
      (fun _ -> ignore (elt#removeChild dialog); true);
    ignore (close#appendChild close_text);
    ignore (title#appendChild close);
    ignore (dialog#appendChild title)

  method add_body =
    let body = (D.document#createElement "div") in
    let style = body#_get_style in
    ignore (style#_set_border "1px solid black");
    ignore (style#_set_borderTop "1px none black");
    ignore (style#_set_height "150px");
    let text = (D.document#createTextNode "" : D.text) in
    text#_set_data "body";
    ignore (body#appendChild text);
    ignore (dialog#appendChild body)

  method decorate =
    self#add_title;
    self#add_body;
    let style = dialog#_get_style in
    ignore (style#_set_display "inline-block");
    ignore (style#_set_position "absolute");
    ignore (style#_set_width "200px");
    let mouse = Fd.mouse_b () in
    Fd.appendChild elt
    (F.blift mouse (fun (x,y) ->
      dialog#_get_style#_set_left (string_of_int x);
      dialog#_get_style#_set_top (string_of_int y);
      dialog))

  initializer self#decorate
end

let log = new logger

let onload () =
  log#log "onload";
  let main = (D.document#getElementById "main" : D.element) in
  ignore (new dialog main)
;;

D.window#_set_onload onload
