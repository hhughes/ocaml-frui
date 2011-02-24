class cloud (c : Dom.canvas) =
object (self)
  val counter = new Counter.counter
  val width = 200
  val height = 200
  method canvas = c
  method counter = counter
  method render_word w t h =
    begin
      let context = c#getContext "2d" in
      context#_set_fillStyle "red";
      context#_set_textBaseline "top";
      context#_set_font (Printf.sprintf "%dpx san-serif" (int_of_float h));
      context#fillText w 2. t
    end
  method render_all =
    begin
      let kvps = counter#kvps in
      let total = float (List.fold_right (fun (k,v) a -> v + a) kvps 0) in
      List.fold_right (
	fun (k,v) t ->
	  let x = (((float v) *. float (height)) /. total) in
	  self#render_word k t x;
	  t +. x) kvps 0.
    end
  method render =
    begin
      let context = c#getContext "2d" in
      context#clearRect 0. 0. 200. 200.;
      self#render_all
    end
  method new_counter name = 
    begin
      counter#add name;
      ignore (Froc.lift (fun v -> self#render) (counter#behavior name))
    end
  method init =
    begin
      c#_set_className "cloud";
      c#_set_width width;
      c#_set_height height
    end
  initializer self#init
end

let init (elt : Dom.element) t0 t1 =
  let cloud_canvas = (Dom.document#createElement "canvas" : Dom.canvas) in
  let c = new cloud cloud_canvas in
  ignore (elt#appendChild c#canvas);
  c
;;
