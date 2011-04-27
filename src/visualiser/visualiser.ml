open Logger
open Msg
open Thread
open Printf
open Fvar

class visualiser (e : Dom.element) t0 t1 m0 m1 =
object (self)
  val threads = Hashtbl.create 10

  method w  = float_of_int (e#_get_offsetWidth - 4) (* would be nice if this were froc-ed *)

  method set_msg_loc msg msg_elt s t0 t1 =
  let d = t1 -. t0 in
  let w = self#w in
  let ts = Msg.timestamp msg in
  let l = ((ts -. s) *. w) /. d in
  ignore (msg_elt#_get_style#_set_left (string_of_int (int_of_float l)))

  method set_fun_loc f msg_elt fs fe ts t0 t1 =
  let fs = if fs < 0. then m0#get else fs in
  let fe = if fe < 0. then m1#get else fe in 
  let d = t1 -. t0 in
  let w = self#w in
  let l = ((fs -. ts) *. w) /. d in
  let wi = ((fe -. fs) *. w) /. d in
  ignore (msg_elt#_get_style#_set_left (string_of_int (int_of_float l)));
  ignore (msg_elt#_get_style#_set_width (string_of_int (int_of_float wi)))

  method set_thread_loc thread_elt ts tf t0 t1 =
  let ts = if ts < 0. then m0#get else ts in
  let tf = if tf < 0. then m1#get else tf in
  let d = t1 -. t0 in
  let w = self#w in
  let l = ((ts -. t0) *. w) /. d in
  let r = ((tf -. t0) *. w) /. d in
  ignore (thread_elt#_get_style#_set_left (string_of_int (int_of_float l)));
  ignore (thread_elt#_get_style#_set_width (string_of_int (int_of_float (r -. l))))

  method create_event_div (thread_elt : Dom.element) classname title =
  let msg_elt = (Dom.document#createElement "div" : Dom.element) in
  ignore (msg_elt#_set_className classname);
  ignore (msg_elt#_set_title title);
  ignore (thread_elt#appendChild msg_elt);
  msg_elt

  method render_msg (thread_elt : Dom.element) thread = function
    | E_msg m ->
      let div = self#create_event_div thread_elt "msg" (Msg.desc m) in
      m#set_froc_loc (Froc.lift3 (self#set_msg_loc m div) thread#start#b t0 t1)
    | E_fn f ->
      let div = self#create_event_div thread_elt "fn" f#name in
      f#set_froc_loc (Froc.lift5 (self#set_fun_loc f div) f#start#b f#finish#b thread#start#b t0 t1)
    | Dummy -> ()

  method render_thread id thread =
  let thread_elt = (Dom.document#createElement "div" : Dom.element) in
  ignore (thread_elt#_set_className "thread");
  ignore (thread_elt#_get_style#_set_top (string_of_int (25 * thread#index)));
  ignore (Froc.lift4 (self#set_thread_loc thread_elt) thread#start#b thread#finish#b t0 t1);
  ignore (e#appendChild thread_elt);
  thread_elt

  method add_to_thread msg =
  (*debug (sprintf "event of type %s" (Msg.ty msg));*)
  let tid = Msg.threadId msg in
  (if not (Hashtbl.mem threads tid) then 
    begin
      let thread = new thread tid in
      Hashtbl.add threads tid (thread);
      let thread_elt = self#render_thread tid thread in
      thread#msgs#lift (self#render_msg thread_elt thread)
    end);
  let thread = Hashtbl.find threads tid in
  thread#parse_msg msg;
  let ts = Msg.timestamp msg in
  if ts >= 0. then
    begin
      if ts < m0#get or m0#get = (-1.) then m0#set ts;
      if ts > m1#get or m1#get = (-1.) then m1#set ts;
    end;
end

let init (elt : Dom.element) t0 t1 m0 m1 =
  let v = new visualiser elt t0 t1 m0 m1 in
  v
