let pi = Javascript.Math.pi

class pie_counter =
object (self)
  val counters = Hashtbl.create 10
  method add (name : string) = Hashtbl.add counters name (new Fvar.fvar 0)
  method get name = (Hashtbl.find counters name)#get
  method set name v = (Hashtbl.find counters name)#set v 
  method inc name = let v = self#get name in Logger.debug (Printf.sprintf "inc to %d" (v+1)); self#set name (v+1)
  method behavior name = (Hashtbl.find counters name)#b
  method values = Hashtbl.fold (fun k v a -> self#get k :: a) counters []
end

class pie (c : Dom.canvas) =
object (self)
  val counter = new pie_counter
  val width = 100
  val height = 100
  method canvas = c
  method counter = counter
  method render_slice s e =
    begin
      let context = c#getContext "2d" in
      context#_set_strokeStyle "red";
      context#beginPath;
      context#arc 50. 50. 28. s e false;
      context#lineTo 50. 50.;
      context#closePath;
      context#stroke
    end
  method render_all =
    begin
      let values = counter#values in
      let total = float (List.fold_right (fun v a -> v + a) values 0) in
      List.fold_right (
	fun v s ->
	  let e = (((float v) *. pi *. 2.) /. total) +. s in
	  self#render_slice s e;
	  e) values 0.
    end
  method render_outline = 
    begin
      let context = c#getContext "2d" in
      context#_set_strokeStyle "black";
      context#beginPath;
      context#arc 50. 50. 30. 0. (pi *. 2.) false; 
      context#closePath;
      context#stroke
    end
  method render = 
    begin
      let context = c#getContext "2d" in
      context#clearRect 0. 0. 100. 100.;
      self#render_outline;
      self#render_all
    end
  method new_counter name = 
    begin
      counter#add name;
      Logger.debug (Printf.sprintf "added %s" name);
      ignore (Froc.lift (fun v -> Logger.debug "render..."; self#render) (counter#behavior name))
    end
  method init =
    begin
      self#canvas#_set_className "pie";
      self#canvas#_set_width width;
      self#canvas#_set_height height;
      self#render_outline
    end
  initializer self#init
end

let init (elt : Dom.element) =
  let pie_canvas = (Dom.document#createElement "canvas" : Dom.canvas) in
  let p = new pie pie_canvas in
  ignore (elt#appendChild p#canvas);
  p
;;