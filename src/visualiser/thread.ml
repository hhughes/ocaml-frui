open Msg
open Logger

class thread i =
object (self)
  val mutable msgs : event list = []
  val mutable id = -1
  val mutable start = -1 (* -1 means the thread start time is unknown *)
  val mutable finish = -1 (* -1 means the thread is yet to finish *)
  method add_fn msg =
    let f = new fn in
    begin
      match Msg.ty msg with
	| "fn_start" -> ignore (f#set_start (Msg.timestamp msg))
	| "fn_finish" -> ignore (f#set_finish (Msg.timestamp msg))
	| _ -> ()
    end;
    f#set_name (Msg.name msg);
    msgs <- (E_fn f) :: msgs
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
		  | "fn_start" -> ignore (f#set_start (Msg.timestamp msg))
		  | "fn_finish" -> ignore (f#set_finish (Msg.timestamp msg))
		  | _ -> ()
	      end
	    | _ -> ()
	end
    with
      | Not_found _ -> self#add_fn msg
  method parse_msg msg = 
    match Msg.ty msg with
      | "t_start" -> start <- (Msg.timestamp msg)
      | "t_finish" -> finish <- (Msg.timestamp msg)
      | "fn_start"
      | "fn_finish" -> self#lookup_fn msg
      | "msg" -> msgs <- (E_msg msg) :: msgs
      | _ -> ()
  method msgs = msgs
  method start = start
  method finish = finish
  initializer id <- i
end
