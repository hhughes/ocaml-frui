open Msg
open Logger
open Fvar

let t_index = ref 0
let next_index () =
  let i = !t_index in
  t_index := i + 1;
  i

class thread i =
object (self)
  val mutable msgs = new Flist.flist
  val mutable id = -1
  val start = new fvar (-1.)
  val finish = new fvar (-1.)
  val froc_loc = Froc.return ()
  val index = next_index ()
  method msg_append msg = msgs#push (Froc.return (msg))
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
	  let e = List.find (fun m -> match Froc.sample m with
	    | E_fn f -> (f#name) = (Msg.name msg)
	    | _ -> false) msgs#list in
	  match (Froc.sample e) with
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
      | Not_found _ -> debug "new function"; self#add_fn msg
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
  method froc_loc = froc_loc
  method index = index
  initializer id <- i
end
