open Logger
open Dialog

module D = Dom
module F = Froc
module Fd = Froc_dom

let onload () =
  debug "onload";
  let main = (D.document#getElementById "main" : D.element) in
  ignore (new dialog main)
;;

D.window#_set_onload onload
