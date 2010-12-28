open Ocamljs.Inline

module D = Dom
module F = Froc
module Fd = Froc_dom

class logger =
object
  method log (msg : string) =  ignore(<:exp< console.log($msg$) >>)
end

let debug = (new logger)#log

let set_text (e : D.element) text =
  let text_node = (D.document#createTextNode "" : D.text) in
  text_node#_set_data text;
  ignore (e#appendChild text_node)

let mouse_down_e (elt : D.element) =
  let e, s = F.make_event () in
  let f_down me = F.send s true in
  let f_up me = F.send s false in
  elt#addEventListener_mouseEvent_ "mousedown" f_down false;
  elt#addEventListener_mouseEvent_ "mouseup" f_up false;
  F.cleanup (fun () -> elt#removeEventListener_mouseEvent_ "mousedown" f_down false; elt#removeEventListener_mouseEvent_ "mouseup" f_up false);
  e

class dialog (elt : D.element) =
object (self)
  val dialog = (D.document#createElement "div")
  val mutable moving = false
  
  method add_title =
    let title = (D.document#createElement "div") in
    let close = (D.document#createElement "div") in
    set_text title "title";
    set_text close "x";
    ignore (title#_get_style#_set_border "1px solid black");
    ignore (close#_get_style#_set_display "inline-block");
    ignore (close#_get_style#_set_position "absolute");
    ignore (close#_get_style#_set_right "5px");
    ignore (close#_get_style#_set_cursor "hand");
    close#_set_onclick
      (fun _ -> ignore (elt#removeChild dialog); true);
    ignore (title#appendChild close);
    ignore (dialog#appendChild title)

  method add_body =
    let body = (D.document#createElement "div") in
    set_text body "body";
    ignore (body#_get_style#_set_border "1px solid black");
    ignore (body#_get_style#_set_borderTop "1px none black");
    ignore (body#_get_style#_set_height "150px");
    ignore (dialog#appendChild body)

  method add_mouse_events =
    let mouse_move = Fd.mouse_b () in
    Fd.appendChild elt
    (F.blift mouse_move (fun (x,y) ->
      if not moving then dialog
      else
	begin
	  dialog#_get_style#_set_left (string_of_int x);
	  dialog#_get_style#_set_top (string_of_int y);
	  dialog
	end));
    let mouse_down = F.hold (false) (mouse_down_e dialog) in
    F.notify_result_b (F.blift mouse_down (fun b -> b)) (fun r -> match r with
      |  F.Value b -> (moving <- b)
      | _ -> ())

  method decorate =
    self#add_title;
    self#add_body;
    self#add_mouse_events;
    ignore (dialog#_get_style#_set_display "inline-block");
    ignore (dialog#_get_style#_set_position "absolute");
    ignore (dialog#_get_style#_set_width "200px");

  initializer self#decorate
end

let onload () =
  debug "onload";
  let main = (D.document#getElementById "main" : D.element) in
  ignore (new dialog main)
;;

D.window#_set_onload onload
