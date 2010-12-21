open Ocamljs.Inline

module D = Dom

class logger =
object
  method log (msg : string) =  ignore(<:exp< console.log($msg$) >>)
end

class dialog (elt : D.element) =
object
  method decorate =
    let text = (D.document#createTextNode "hw_text" : D.text) in
    text#_set_data "This is a dialog!";
    ignore (elt#appendChild text)
end

let log = new logger

let onload () =
  log#log "onload";
  let main = (D.document#getElementById "main" : D.element) in
  let d = new dialog main in
  d#decorate
;;

D.window#_set_onload onload
