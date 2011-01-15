open Javascript

class type msg =
object
(*
  method timestamp : int
  method name : string
  method desc : string
  method misc : 'a*)
end

module Msg =
struct
  external ty : msg -> string = ".ty"
end

external new_Object : string -> msg = "@eval"

let unmarshall_json json = new_Object json
  
