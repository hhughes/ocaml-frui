open Fvar

class msg_count (e : Dom.element) =
object (self)
  val mc = new fvar 0
  method inc = mc#set (mc#get + 1)
  method init =
    let count_txt = (Dom.document#createTextNode "" : Dom.text ) in
    ignore (Froc.lift (fun c -> ignore (count_txt#_set_data (string_of_int c))) mc#b);
    ignore (e#appendChild count_txt);

  initializer self#init
end

let init (e : Dom.element) t0 t1 =
  let m = new msg_count e in
  m
