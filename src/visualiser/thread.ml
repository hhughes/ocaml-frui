open Msg
open Logger
open Fvar

class thread i =
object (self)
  val mutable msgs : event list = []
  val mutable id = -1
  val start = new fvar (-1)
  val finish = new fvar (-1)
  val latest_msg = new fvar (Dummy)
  method msg_append msg =
    latest_msg#set msg;
    msgs <- msg :: msgs
  method add_fn msg =
    let f = new fn in
    begin
      match Msg.ty msg with
	| "fn_start" -> ignore (f#start#set (Msg.timestamp msg))
	| "fn_finish" -> ignore (f#finish#set (Msg.timestamp msg))
	| _ -> ()
    end;
    f#set_name (Msg.name msg);
    self#msg_append (E_fn f)
  method lookup_fn msg =
    try
      	begin
	  let e = List.find (fun m -> match m with
	    | E_fn f -> (f#name) = (Msg.name msg)
	    | _ -> false) msgs in
	  match e with
	    | E_fn f ->
	      begin
		match Msg.ty msg with
		  | "fn_start" -> ignore (f#start#set (Msg.timestamp msg))
		  | "fn_finish" -> ignore (f#finish#set (Msg.timestamp msg))
		  | _ -> ()
	      end
	    | _ -> ()
	end
    with
      | Not_found _ -> self#add_fn msg
  method parse_msg msg = 
    match Msg.ty msg with
      | "t_start" -> start#set (Msg.timestamp msg)
      | "t_finish" -> finish#set (Msg.timestamp msg)
      | "fn_start"
      | "fn_finish" -> self#lookup_fn msg
      | "msg" -> self#msg_append (E_msg msg)
      | _ -> ()
  method msgs = msgs
  method start = start
  method finish = finish
  method latest_msg = latest_msg
  initializer id <- i
end
