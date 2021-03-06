open Javascript
open Fvar

class msg =
object
  val mutable froc_loc = Froc.return ()
  method froc_loc = froc_loc
  method set_froc_loc f = froc_loc <- f
end

class fn =
object
  inherit msg
  val mutable start = new fvar (-1.)
  val mutable finish = new fvar (-1.)
  val mutable name = ""
  method set_name n = name <- n
  method name = name
  method start = start
  method finish = finish
end

type event = E_msg of msg | E_fn of fn | Dummy

module Msg =
struct
  external threadId : msg -> int = ".tid"
  external ty : msg -> string = ".ty"
  external timestamp : msg -> float = ".ts"
  external name : msg -> string = ".name"
  external desc : msg -> string = ".desc"
  external misc : msg -> 'a = ".misc"
end

external new_Object : string -> msg js_array = "@eval"

let unmarshall_json json = new_Object json

let rec js_to_list msgs =
  if msgs#_get_length > 0 then
    msgs#pop :: (js_to_list msgs)
  else []
  
