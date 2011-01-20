open Logger
open Dialog

module D = Dom
module F = Froc
module Fd = Froc_dom

let onload () =
  debug "onload";
  let main = (D.document#getElementById "main" : D.element) in
  let new_dlg = (D.document#createElement "button" : D.button) in
  ignore (set_text (new_dlg :> D.element) "add dialog");
  ignore (main#appendChild new_dlg);
  new_dlg#_set_onclick (fun _ -> ignore (new dialog main ()); true)
;;

D.window#_set_onload onload
