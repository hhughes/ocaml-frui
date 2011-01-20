open Javascript

class type msg = object end

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
  
