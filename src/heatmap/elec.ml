open Javascript

class elec = object end

module Elec =
struct
  external room : elec -> string = ".room"
  external data : elec -> string js_array js_array = ".data"
end
