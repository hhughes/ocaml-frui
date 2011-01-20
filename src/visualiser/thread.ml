open Msg

class thread i =
object
  val mutable msgs : msg list = []
  val mutable id = -1
  val mutable start = -1 (* -1 means the thread start time is unknown *)
  val mutable finish = -1 (* -1 means the thread is yet to finish *)
  method set_msgs m = msgs <- m
  method msgs = msgs
  method start = start
  method finish = finish
  initializer id <- i
end
