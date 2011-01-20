open Msg
open Logger

class thread i =
object
  val mutable msgs : msg list = []
  val mutable id = -1
  val mutable start = -1 (* -1 means the thread start time is unknown *)
  val mutable finish = -1 (* -1 means the thread is yet to finish *)
  method parse_msg msg = 
    (match Msg.ty msg with
      | "t_start" -> start <- (Msg.timestamp msg)
      | "t_finish" -> finish <- (Msg.timestamp msg)
      | _ -> ());
	debug (string_of_int start);
	debug (Msg.ty msg);
    msgs <- msg :: msgs
  method msgs = msgs
  method start = start
  method finish = finish
  initializer id <- i
end
