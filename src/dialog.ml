module D = Dom
module F = Froc
module Fd = Froc_dom

type dlgstate =
    Normal
  | Moving of int * int
  | Resizing of int * int

let set_text (e : D.element) text =
  let text_node = (D.document#createTextNode "" : D.text) in
  text_node#_set_data text;
  ignore (e#appendChild text_node)

let mouse_down_if_e p (elt : D.element) =
  let e, s = F.make_event () in
  let f_down me = if p me then F.send s (true, me#_get_clientX - elt#_get_offsetLeft, me#_get_clientY - elt#_get_offsetTop) in
  let f_up me = if p me then F.send s (false, me#_get_clientX - elt#_get_offsetLeft, me#_get_clientY - elt#_get_offsetTop) in
  elt#addEventListener_mouseEvent_ "mousedown" f_down false;
  elt#addEventListener_mouseEvent_ "mouseup" f_up false;
  F.cleanup (fun () -> elt#removeEventListener_mouseEvent_ "mousedown" f_down false; elt#removeEventListener_mouseEvent_ "mouseup" f_up false);
  e

let mouse_down_e (elt : D.element) = mouse_down_if_e (fun x -> true) elt

class dialog (elt : D.element) =
object (self)
  val dialog = (D.document#createElement "div")
  val mutable state = Normal
  
  method add_title =
    let title = (D.document#createElement "div") in
    let close = (D.document#createElement "div") in
    set_text title "title";
    set_text close "x";
    ignore (title#_get_style#_set_border "1px solid black");
    ignore (title#_get_style#_set_display "absolute");
    ignore (title#_get_style#_set_left "0");
    ignore (title#_get_style#_set_top "0");
    ignore (title#_get_style#_set_right "0");
    ignore (title#_get_style#_set_height "20px");
    ignore (close#_get_style#_set_display "inline-block");
    ignore (close#_get_style#_set_position "absolute");
    ignore (close#_get_style#_set_right "5px");
    ignore (close#_get_style#_set_cursor "hand");
    close#_set_onclick
      (fun _ -> self#hide; true);
    ignore (title#appendChild close);
    ignore (dialog#appendChild title)

  method add_body =
    let body = (D.document#createElement "div") in
    set_text body "body";
    ignore (body#_get_style#_set_position "absolute");
    ignore (body#_get_style#_set_border "1px solid black");
    ignore (body#_get_style#_set_borderTop "1px none black");
    ignore (body#_get_style#_set_left "0");
    ignore (body#_get_style#_set_top "20px");
    ignore (body#_get_style#_set_right "0");
    ignore (body#_get_style#_set_bottom "0");
    ignore (dialog#appendChild body)

  method add_mouse_events =
    let mouse_move = Fd.mouse_b () in
    Fd.appendChild elt
    (F.blift mouse_move (fun (x,y) ->
      match state with
	| Moving (o_x, o_y) ->
	  begin
	    dialog#_get_style#_set_left (string_of_int (x - o_x));
	    dialog#_get_style#_set_top (string_of_int (y - o_y));
	    dialog
	  end
	| Resizing (o_x, o_y) ->
	  begin
	    let l = dialog#_get_offsetLeft in
	    let t = dialog#_get_offsetTop in
	    dialog#_get_style#_set_width (string_of_int (x - l + 5));
	    dialog#_get_style#_set_height (string_of_int (y - t + 5));
	    dialog
	  end
	| _ -> dialog
     ));
    let is_resize me =
      let x = me#_get_clientX in
      let y = me#_get_clientY in
      let l = dialog#_get_offsetLeft in
      let t = dialog#_get_offsetTop in
      let w = dialog#_get_offsetWidth in
      let h = dialog#_get_offsetHeight in
      x > (l+w)-20 && y > (t+h)-20
    in
    let mouse_down_move = F.hold (false,0,0) (mouse_down_if_e (fun me -> not (is_resize me)) dialog) in
    F.notify_result_b (F.blift mouse_down_move (fun a -> a)) (fun r -> match r with
      |  F.Value (b,x,y) -> (state <- if b then Moving (x,y) else Normal)
      | _ -> ());
    let mouse_down_resize = F.hold (false,0,0) (mouse_down_if_e (fun me -> is_resize me) dialog) in
    F.notify_result_b (F.blift mouse_down_resize (fun a -> a)) (fun r -> match r with
      |  F.Value (b,x,y) -> (state <- if b then Resizing (x,y) else Normal)
      | _ -> ())

  method hide = ignore (elt#removeChild dialog);

  method decorate =
    self#add_title;
    self#add_body;
    self#add_mouse_events;
    ignore (dialog#_get_style#_set_display "inline-block");
    ignore (dialog#_get_style#_set_position "absolute");
    ignore (dialog#_get_style#_set_width "200px");
    ignore (dialog#_get_style#_set_height "150px");

  initializer self#decorate
end
