open Javascript

class msg = object end

class fn =
object
  inherit msg
  val mutable start = -1
  val mutable finish = -1
  val mutable name = ""
  method set_start t = start <- t
  method set_finish t = finish <- t
  method set_name n = name <- n
  method name = name
  method start = start
  method finish = finish
end

type event = E_msg of msg | E_fn of fn

module Msg =
struct
  external threadId : msg -> int = ".tid"
  external ty : msg -> string = ".ty"
  external timestamp : msg -> int = ".ts"
  external name : msg -> string = ".name"
  external desc : msg -> string = ".desc"
  external misc : msg -> 'a = ".misc"
end

external new_Object : string -> msg js_array = "@eval"

let unmarshall_json json = new_Object json

let rec for_each f msgs =
  if msgs#_get_length > 0 then
    begin
      f msgs#pop;
      for_each f msgs
    end
  
