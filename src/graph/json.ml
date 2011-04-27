open Javascript

class datum = object end
class item = object end

module Datum =
struct
  external indicator : datum -> item = ".indicator"
  external country : datum -> item = ".country"
  external value : datum -> string = ".value"
  external date : datum -> string = ".date"
end

module Item =
struct
  external id : item -> string = ".id"
  external value : item -> string = ".value"
end

external new_Object : string -> datum js_array js_array = "@eval"

let unmarshall_json json = new_Object json

let rec js_to_list msgs =
  if msgs#_get_length > 0 then
    msgs#pop :: (js_to_list msgs)
  else []
